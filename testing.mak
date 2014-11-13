#	$Id$
#
#	Copyright (c) 1996-2002, Darren Hiebert
#
#	Development makefile for Exuberant Ctags, used to build releases.
#	Requires GNU make.

CTAGS_TEST = ./ctags
CTAGS_REF = ./ctags.ref
TEST_OPTIONS = -nu --c-kinds=+lpx
FUZZ_TIMEOUT=10
# You can specify one of language listed in $(./ctags --list-languages).
FUZZ_LANGUAGE=
FUZZ_SRC_DIRS=

DIFF = $(call DIFF_BASE,tags.ref,tags.test,$(DIFF_FILE))

.PHONY: test test.include test.fields test.extra test.linedir test.etags test.eiffel test.linux test.units units fuzz clean clean-test clean-units

test: test.include test.fields test.extra test.linedir test.etags test.eiffel test.linux test.units units

test.%: DIFF_FILE = $@.diff

REF_INCLUDE_OPTIONS = $(TEST_OPTIONS) --format=1
TEST_INCLUDE_OPTIONS = $(TEST_OPTIONS) --format=1
test.include: $(CTAGS_TEST) $(CTAGS_REF)
	@ printf '%-60s' "Testing tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_INCLUDE_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_INCLUDE_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_FIELD_OPTIONS = $(TEST_OPTIONS) --fields=+afmikKlnsSz
TEST_FIELD_OPTIONS = $(TEST_OPTIONS) --fields=+afmikKlnsStz
FEAtest.fields: $(CTAGS_TEST) $(CTAGS_REF)
	@ printf '%-60s' "Testing extension fields..."
	@ $(CTAGS_REF) -R $(REF_FIELD_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_FIELD_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_EXTRA_OPTIONS = $(TEST_OPTIONS) --extra=+fq --format=1
TEST_EXTRA_OPTIONS = $(TEST_OPTIONS) --extra=+fq --format=1
test.extra: $(CTAGS_TEST) $(CTAGS_REF)
	@ printf '%-60s' "Testing extra tags..."
	@ $(CTAGS_REF) -R $(REF_EXTRA_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_EXTRA_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_LINEDIR_OPTIONS = $(TEST_OPTIONS) --line-directives -n
TEST_LINEDIR_OPTIONS = $(TEST_OPTIONS) --line-directives -n
test.linedir: $(CTAGS_TEST) $(CTAGS_REF)
	@ printf '%-60s' "Testing line directives..."
	@ $(CTAGS_REF) $(REF_LINEDIR_OPTIONS) -o tags.ref Test/line_directives.c
	@ $(CTAGS_TEST) $(TEST_LINEDIR_OPTIONS) -o tags.test Test/line_directives.c
	@- $(DIFF)

REF_ETAGS_OPTIONS = -e
TEST_ETAGS_OPTIONS = -e
test.etags: $(CTAGS_TEST) $(CTAGS_REF)
	@ printf '%-60s' "Testing TAGS output..."
	@ $(CTAGS_REF) -R $(REF_ETAGS_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_ETAGS_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_EIFFEL_OPTIONS = $(TEST_OPTIONS) --format=1 --languages=eiffel
TEST_EIFFEL_OPTIONS = $(TEST_OPTIONS) --format=1 --languages=eiffel
EIFFEL_DIRECTORY = $(ISE_EIFFEL)/library/base
HAVE_EIFFEL := $(shell ls -dtr $(EIFFEL_DIRECTORY) 2>/dev/null)
ifeq ($(HAVE_EIFFEL),)
test.eiffel:
	@ echo "No Eiffel library source found for testing"
else
test.eiffel: $(CTAGS_TEST) $(CTAGS_REF)
	@ printf '%-60s' "Testing Eiffel tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_EIFFEL_OPTIONS) -o tags.ref $(EIFFEL_DIRECTORY)
	@ $(CTAGS_TEST) -R $(TEST_EIFFEL_OPTIONS) -o tags.test $(EIFFEL_DIRECTORY)
	@- $(DIFF)
endif

REF_LINUX_OPTIONS = $(TEST_OPTIONS) --fields=k
TEST_LINUX_OPTIONS = $(TEST_OPTIONS) --fields=k
LINUX_KERNELS_DIRECTORY :=
LINUX_DIRECTORY := $(shell find $(LINUX_KERNELS_DIRECTORY) -maxdepth 1 -type d -name 'linux-[1-9]*' 2>/dev/null | tail -1)
ifeq ($(LINUX_DIRECTORY),)
test.linux:
	@ echo "No Linux kernel source found for testing"
else
test.linux: $(CTAGS_TEST) $(CTAGS_REF)
	@ printf '%-60s' "Testing Linux tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_LINUX_OPTIONS) -o tags.ref $(LINUX_DIRECTORY)
	@ $(CTAGS_TEST) -R $(TEST_LINUX_OPTIONS) -o tags.test $(LINUX_DIRECTORY)
	@- $(DIFF)
endif

TEST_ARTIFACTS = test.*.diff tags.ref ctags.ref.exe tags.test
clean: clean-test clean-units
clean-test:
	rm -f $(TEST_ARTIFACTS)

#
# FUZZ Target
#
HAVE_TIMEOUT := $(shell which timeout 2>/dev/null)
HAVE_FIND    := $(shell which find 2>/dev/null)
ifeq ($(HAVE_TIMEOUT),)
fuzz:
	@ echo "No timeout command of GNU coreutils found"
else ifeq ($(HAVE_FIND),)
fuzz:
	@ echo "No timeout command of find found"
else

define run-fuzz-ctags
	if ! timeout -s INT $(FUZZ_TIMEOUT) \
		$(CTAGS_TEST) --language-force=$1 -o - $2 \
		> /dev/null 2>&1; then \
		echo Fuzz testing failure: lang: $1 input: $2; \
	fi
endef

fuzz: $(CTAGS_TEST)
	@ \
	for lang in $$($(CTAGS_TEST) --list-languages); do \
		if test -z "$(FUZZ_LANGUAGE)" || test "$(FUZZ_LANGUAGE)" = "$${lang}"; then \
			echo "Fuzz-testing: $${lang}"; \
			for input in Test/* Units/*.[dbt]/input.*; do \
				$(call run-fuzz-ctags,"$${lang}","$${input}"); \
			done; \
			for d in $(FUZZ_SRC_DIRS); do \
				find "$$d" -type f \
				| while read input; do \
					$(call run-fuzz-ctags,"$${lang}","$${input}"); \
				done; \
			done; \
		fi ; \
	done
endif

#
# SHELL must be dash or bash.
#
ifdef VG
UNITS_VALGRIND=--with-valgrind
endif
UNITS_TIMEOUT=0
UNIT_LANGUAGES=
UNITS=

test.units: units
units: $(CTAGS_TEST)
	@ \
	c="misc/units run --languages=$(UNIT_LANGUAGES) --units=$(UNITS) $(UNITS_VALGRIND) --with-timeout=$(UNITS_TIMEOUT)"; \
	success=true; \
	$(SHELL) $${c} Units; \
	[ $$? -eq 0 ]  || success=false; \
	$$success
clean-units:
	$(SHELL) misc/units clean Units

# Local Variables:
# Mode: makefile
# End:
# vi:ts=4 sw=4
