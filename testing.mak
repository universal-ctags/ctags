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

DIFF_OPTIONS = -U 0 -I '^!_TAG' --strip-trailing-cr
DIFF = $(call DIFF_BASE,tags.ref,tags.test,$(DIFF_FILE))
DIFF_BASE = if diff $(DIFF_OPTIONS) $1 $2 > $3; then \
		rm -f $1 $2 $3 $4; \
		echo "passed" ; \
		true ; \
	  else \
		echo "FAILED" ; \
		echo "	differences left in $3" ; \
		false ; \
	  fi

CHECK_FEATURES = (\
	while read; do \
		found=no; \
		for f in $$( $(CTAGS_TEST) --list-features); do \
			if test "$$REPLY" = "$$f"; then found=yes; fi; \
		done; \
		if ! test $$found = yes; then \
			echo "skipped (required feature $$REPLY is not available)"; \
			exit 1; \
		fi; \
	done < $1; \
	exit 0; \
)
#
# Run unit test s under valgrind:
#
# 	$ make -f testing.mak VG=1 test.units
#
VALGRIND_COMMAND       = valgrind
VALGRIND_OPTIONS       = --quiet --leak-check=full --show-leak-kinds=all
VALGRIND_EXTRA_OPTIONS =
ifdef VG
VALGRIND = $(VALGRIND_COMMAND) $(VALGRIND_OPTIONS) $(VALGRIND_EXTRA_OPTIONS)
endif

.PHONY: test test.include test.fields test.extra test.linedir test.etags test.eiffel test.linux test.units fuzz

test: test.include test.fields test.extra test.linedir test.etags test.eiffel test.linux test.units

test.%: DIFF_FILE = $@.diff

REF_INCLUDE_OPTIONS = $(TEST_OPTIONS) --format=1
TEST_INCLUDE_OPTIONS = $(TEST_OPTIONS) --format=1
test.include: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_INCLUDE_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_INCLUDE_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_FIELD_OPTIONS = $(TEST_OPTIONS) --fields=+afmikKlnsSz
TEST_FIELD_OPTIONS = $(TEST_OPTIONS) --fields=+afmikKlnsStz
FEAtest.fields: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing extension fields..."
	@ $(CTAGS_REF) -R $(REF_FIELD_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_FIELD_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_EXTRA_OPTIONS = $(TEST_OPTIONS) --extra=+fq --format=1
TEST_EXTRA_OPTIONS = $(TEST_OPTIONS) --extra=+fq --format=1
test.extra: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing extra tags..."
	@ $(CTAGS_REF) -R $(REF_EXTRA_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_EXTRA_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_LINEDIR_OPTIONS = $(TEST_OPTIONS) --line-directives -n
TEST_LINEDIR_OPTIONS = $(TEST_OPTIONS) --line-directives -n
test.linedir: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing line directives..."
	@ $(CTAGS_REF) $(REF_LINEDIR_OPTIONS) -o tags.ref Test/line_directives.c
	@ $(CTAGS_TEST) $(TEST_LINEDIR_OPTIONS) -o tags.test Test/line_directives.c
	@- $(DIFF)

REF_ETAGS_OPTIONS = -e
TEST_ETAGS_OPTIONS = -e
test.etags: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing TAGS output..."
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
	@ echo -n "Testing Eiffel tag inclusion..."
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
	@ echo -n "Testing Linux tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_LINUX_OPTIONS) -o tags.ref $(LINUX_DIRECTORY)
	@ $(CTAGS_TEST) -R $(TEST_LINUX_OPTIONS) -o tags.test $(LINUX_DIRECTORY)
	@- $(DIFF)
endif


UNITS_ARTIFACTS=Units/*.d/EXPECTED.TMP Units/*.d/OUTPUT.TMP Units/*.d/DIFF.TMP
test.units: $(CTAGS_TEST)
	@ \
	success=true; \
	for input in Units/*.d/input.*; do \
		t=$${input%/input.*}; \
		name=$${t%.d}; \
		\
		expected="$$t"/expected.tags; \
		expectedtmp="$$t"/EXPECTED.TMP; \
		args="$$t"/args.ctags; \
		filter="$$t"/filter; \
		output="$$t"/OUTPUT.TMP; \
		diff="$$t"/DIFF.TMP; \
		stderr="$$t"/STDERR.TMP; \
		features="$$t"/features; \
		\
		echo -n "Testing $${name}..."; \
		\
		if test -e "$$features"; then \
			if ! $(call CHECK_FEATURES, "$$features"); then \
				continue; \
			fi; \
		fi; \
		$(VALGRIND) $(CTAGS_TEST) --options=NONE --data-dir=Data --data-dir=+$$t -o - \
		$$(test -f "$${args}" && echo "--options=$${args}") \
		"$$input" 2> "$$stderr" | \
		if test -x "$$filter"; then "$$filter"; else cat; fi > "$${output}";	\
		cp "$$expected" "$$expectedtmp"; \
		$(call DIFF_BASE,"$$expectedtmp","$$output","$$diff","$$stderr"); \
		test $$? -eq 0 || { echo "	cmdline: " \
					$(CTAGS_TEST) --options=NONE --data-dir=Data --data-dir=+$$t -o - \
					$$(test -f "$${args}" && echo "--options=$${args}") "$$input" ;\
				    success=false; }; \
	done; \
	$$success

TEST_ARTIFACTS = test.*.diff tags.ref ctags.ref.exe tags.test $(UNITS_ARTIFACTS)
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

define run-ctags
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
			for input in Test/* Units/*.d/input.*; do \
				$(call run-ctags,"$${lang}","$${input}"); \
			done; \
			for d in $(FUZZ_SRC_DIRS); do \
				find "$$d" -type f \
				| while read input; do \
					$(call run-ctags,"$${lang}","$${input}"); \
				done; \
			done; \
		fi ; \
	done
endif

# Local Variables:
# Mode: makefile
# End:
# vi:ts=4 sw=4
