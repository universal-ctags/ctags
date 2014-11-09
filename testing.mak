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
UNIT_LANGUAGE=
FUZZ_SRC_DIRS=

DIFF_OPTIONS = -U 0 -I '^!_TAG' --strip-trailing-cr
DIFF = $(call DIFF_BASE,tags.ref,tags.test,$(DIFF_FILE))
DIFF_BASE = if diff $(DIFF_OPTIONS) $1 $2 > $3; then \
		rm -f $1 $2 $3 $4; \
		echo "passed" ; \
		n_passed=$$(expr $$n_passed + 1); \
		true ; \
	  elif test -d $(5).b; then \
		echo "failed but KNOWN bug" ; \
		n_known_bugs=$$(expr $$n_known_bugs + 1); \
		true ; \
	  else \
		echo "FAILED" ; \
		echo "	differences left in $3" ; \
		n_failed=$$(expr $$n_failed + 1); \
		failed_cases="$$failed_cases $5" ; \
		false ; \
	  fi

DEFINE_CHECK_LANGUAGES  =\
check_languages()\
{       local expected; \
	while read expected; do \
		found=no; \
		for f in $$( $(UNIT_CTAGS_CMDLINE) --list-languages 2>/dev/null | sed -e 's/ //' ) ; do \
			if test "$$expected" = "$$f"; then found=yes; fi; \
		done; \
		if ! test $$found = yes; then \
			n_skipped_languages=$$(expr $$n_skipped_languages + 1); \
			echo "skipped (required language parser $$expected is not available)"; \
			return 1; \
		fi; \
	done < $$1; \
	return 0; \
}

DEFINE_CHECK_FEATURES =\
check_features()\
{       local expected; \
	while read expected; do \
		found=no; \
		for f in $$( $(CTAGS_TEST) --list-features); do \
			if test "$$expected" = "$$f"; then found=yes; fi; \
		done; \
		if ! test $$found = yes; then \
			n_skipped_features=$$(expr $$n_skipped_features + 1); \
			echo "skipped (required feature $$expected is not available)"; \
			return 1; \
		fi; \
	done < $$1; \
	return 0; \
}

#
# Run unit test s under valgrind:
#
# 	$ make -f testing.mak VG=1 test.units
#
VALGRIND_COMMAND       = valgrind
VALGRIND_OPTIONS       = --quiet --leak-check=full
VALGRIND_EXTRA_OPTIONS =

ifdef VG
VALGRIND = $(VALGRIND_COMMAND) $(VALGRIND_OPTIONS) $(VALGRIND_EXTRA_OPTIONS)
ifeq ($(SHELL),/bin/bash)
define STDERR
	2> >(grep -v 'No options will be read from files or environment'  | \
		tee $(1).vg | tee $(1) 1>&2; \
		if ! grep -q "^==" $(1).vg; then \
			rm $(1).vg; \
		fi; \
		)
endef
else
define STDERR
endef
endif
else
define STDERR
	2> $1
endef
endif

.PHONY: test test.include test.fields test.extra test.linedir test.etags test.eiffel test.linux test.units units fuzz clean clean-test clean-units

test: test.include test.fields test.extra test.linedir test.etags test.eiffel test.linux test.units

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


UNITS_ARTIFACTS=Units/*.[dbt]/{EXPECTED.TMP,OUTPUT.TMP,DIFF.TMP,STDERR.TMP,STDERR.TMP.vg}
UNIT_CTAGS_CMDLINE=$(call unit-ctags-cmdline)
define unit-ctags-cmdline
	$(CTAGS_TEST) --options=NONE --libexec-dir=libexec --libexec-dir=+$$t --data-dir=data --data-dir=+$$t -o - \
		$$(test -f "$${args}" && echo "--options=$${args}")
endef

test.units: $(CTAGS_TEST)
	@ \
	success=true; \
	n_passed=0; \
	failed_cases=; \
	n_failed=0; \
	n_skipped_features=0; \
	n_skipped_languages=0; \
	n_skipped_infinite_loops=0; \
	n_known_bugs=0; \
	if [ -n "$(VALGRIND)" -a -n "$(UNIT)" ]; then \
		rm -f Units/$(UNIT).[dbt]/STDERR.TMP.vg; \
	elif [ -n "$(VALGRIND)" ]; then \
		rm -f Units/*.[dbt]/STDERR.TMP.vg; \
	fi; \
	$(DEFINE_CHECK_FEATURES); \
	$(DEFINE_CHECK_LANGUAGES); \
	for input in $$(ls Units/*.[dbti]/input.* | grep -v "~$$"); do \
		t=$${input%/input.*}; \
		name=$${t%.[dbti]}; \
		\
		if test -n "$(UNIT)" -a "$${name}" != "Units/$(UNIT)"; then continue; fi; \
		if test -n "$(UNIT_LANGUAGE)"; then \
			l="$$($(UNIT_CTAGS_CMDLINE) --guess-parser "$$input" 2>/dev/null | sed -nr 's/^.*: (.+)$$/\1/p')"; \
			test "$$l" = "$(UNIT_LANGUAGE)" || continue; \
		fi; \
		\
		expected="$$t"/expected.tags; \
		expectedtmp="$$t"/EXPECTED.TMP; \
		args="$$t"/args.ctags; \
		filter="$$t"/filter; \
		output="$$t"/OUTPUT.TMP; \
		diff="$$t"/DIFF.TMP; \
		stderr="$$t"/STDERR.TMP; \
		features="$$t"/features; \
		languages="$$t"/languages; \
		\
		printf '%-60s' "Testing $${name}"; \
		\
		ext=$${t##*${name}.}; \
		if test "$${ext}" = "i"; then \
			echo "skipped (infinite loop)"; \
			n_skipped_infinite_loops=$$(expr $$n_skipped_infinite_loops + 1); \
			continue; \
		fi; \
		\
		if test -e "$$features"; then \
			if ! check_features "$$features"; then \
				continue; \
			fi; \
		fi; \
		if test -e "$$languages"; then \
			if ! check_languages "$$languages"; then \
				continue; \
			fi; \
		fi; \
		$(VALGRIND) $(UNIT_CTAGS_CMDLINE) \
		"$$input" $(call STDERR,"$$stderr") | \
		if test -x "$$filter"; then "$$filter"; else cat; fi > "$${output}";	\
		cp "$$expected" "$$expectedtmp"; \
		$(call DIFF_BASE,"$$expectedtmp","$$output","$$diff","$$stderr","$$name"); \
		test $$? -eq 0 || { echo "	cmdline: " \
					$(UNIT_CTAGS_CMDLINE) "$$input" ;\
				    success=false; }; \
	done; \
	echo; \
	echo '  Summary of "Units" test'; \
	echo '  -------------------------'; \
	echo '	#passed: ' $$n_passed; \
	echo '	#failed: ' $$n_failed; \
	for f in $$failed_cases; do echo "		$$f"; done; \
	echo '	#skipped(features): ' $$n_skipped_features; \
	echo '	#skipped(languages): ' $$n_skipped_languages; \
	echo '	#skipped(infinite loop): ' $$n_skipped_infinite_loops; \
	echo '	#known-bugs: ' $$n_known_bugs; \
	if [ -n "$(VALGRIND)" -a "$(SHELL)" = /bin/bash -a -n "$(UNIT)" ]; then \
		if [ -f Units/$(UNIT).[dbt]/STDERR.TMP.vg ]; then\
			echo '	#valgrind: 1'; \
			echo '		$(UNIT)'; \
		else \
			echo '	#valgrind: 0'; \
		fi; \
	elif [ -n "$(VALGRIND)" -a "$(SHELL)" = /bin/bash ]; then \
		echo '	#valgrind: ' $$(find Units -name 'STDERR.TMP.vg' |  wc -l); \
		find Units -name 'STDERR.TMP.vg' | \
		xargs -n 1 dirname 2>/dev/null | \
		xargs -n 1 basename 2>/dev/null | \
		sed -e 's/\(.*\)\../\1/' | \
		xargs -n 1 echo "		"; \
	fi; \
	echo; \
	$$success

TEST_ARTIFACTS = test.*.diff tags.ref ctags.ref.exe tags.test $(UNITS_ARTIFACTS)
clean: clean-test
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
units: $(CTAGS_TEST)
	@ \
	c="misc/units run --language=$(UNIT_LANGUAGE) --unit=$(UNIT) $(UNITS_VALGRIND)"; \
	success=true; \
	$(SHELL) $${c} Units; \
	[ $$? -eq 0 ]  || success=false; \
	$$success
clean-units:
	\
	c="misc/units clean"; \
	$(SHELL) $${c} Units; \
	true

# Local Variables:
# Mode: makefile
# End:
# vi:ts=4 sw=4
