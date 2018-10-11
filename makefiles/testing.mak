# -*- makefile -*-
.PHONY: check units fuzz noise tmain tinst clean-units clean-tmain clean-gcov run-gcov codecheck cppcheck dicts cspell verify-units-inputs clean-verify-units-inputs

check: tmain units

clean-local: clean-units clean-tmain clean-verify-units-inputs

CTAGS_TEST = ./ctags$(EXEEXT)
READ_TEST = ./readtags$(EXEEXT)

if HAVE_TIMEOUT
TIMEOUT = 1
else
TIMEOUT = 0
endif

LANGUAGES=
CATEGORIES=
UNITS=

SILENT = $(SILENT_@AM_V@)
SILENT_ = $(SILENT_@AM_DEFAULT_V@)
SILENT_0 = @

V_RUN = $(V_RUN_@AM_V@)
V_RUN_ = $(V_RUN_@AM_DEFAULT_V@)
V_RUN_0 = @echo "  RUN      $@";

#
# FUZZ Target
#
# SHELL must be dash or bash.
#
fuzz: $(CTAGS_TEST)
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	c="$(srcdir)/misc/units fuzz \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		$${VALGRIND} --run-shrink \
		--with-timeout=$(TIMEOUT)"; \
	$(SHELL) $${c} $(srcdir)/Units

#
# NOISE Target
#
noise: $(CTAGS_TEST)
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	c="$(srcdir)/misc/units noise \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		$${VALGRIND} --run-shrink \
		--with-timeout=$(TIMEOUT)"; \
	$(SHELL) $${c} $(srcdir)/Units

#
# CHOP Target
#
chop: $(CTAGS_TEST)
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	c="$(srcdir)/misc/units chop \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		$${VALGRIND} --run-shrink \
		--with-timeout=$(TIMEOUT)"; \
	$(SHELL) $${c} $(srcdir)/Units
slap: $(CTAGS_TEST)
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	c="$(srcdir)/misc/units slap \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		$${VALGRIND} --run-shrink \
		--with-timeout=$(TIMEOUT)"; \
	$(SHELL) $${c} $(srcdir)/Units

TEST_DIRS =

# bmake does not support the metaprogramming similar to gnu make.
if !USING_BMAKE

# TODO: A workaround for automake errorrs.
# TODO: Explain more why this is necessary
set = $(eval $(1) = $(2))

# Parameters:
# $(1): Language Name. For example: PUPPET
# $(2): Extension. For example: .pp
# $(3): Directory Name. For example: parser-puppetManifest.r
# $(4): Command: For example: puppet apply --noop $$$$<  1>/dev/null
define VERIFY_GIVEN_UNITS_TEST_DIR

# Find the test directories where ctags is expected to succeed. Only tests with
# an expected.tags file present are required to have valid input.
$$(call set,$(1)_TEST_DIRS,$$(foreach path, \
    $$(shell find $$(srcdir)/Units/$(3) -name expected.tags), \
    $$(shell dirname $$(path))) )
$$(call set,VERIFY_$(1)_TEST_DIRS_TARGETS,$$($(1)_TEST_DIRS:%=%/.input$(2).verified))

# Add the dependency to the verify-units-inputs phony target for the current language.
verify-units-inputs: $$(VERIFY_$(1)_TEST_DIRS_TARGETS)

# A inner define to verify each file of the specified language.
define VERIFY_ONE_$(1)_TEST_DIR
$$(1)/.input$(2).verified: $$(1)/input$(2)
	$(4) && \
	touch $$$$@
endef

# Generate the inner define
$$(foreach $(1)_test_dir,$$($(1)_TEST_DIRS),$$(eval $$(call VERIFY_ONE_$(1)_TEST_DIR,$$($(1)_test_dir))))

endef # define VERIFY_GIVEN_UNITS_TEST_DIR

# Generate unit test input.xx file verifiers for each language.
$(eval $(call VERIFY_GIVEN_UNITS_TEST_DIR,PUPPET,.pp,parser-puppetManifest.r,puppet apply --noop $$$$< 1>/dev/null))
$(eval $(call VERIFY_GIVEN_UNITS_TEST_DIR,JSON,.json,simple-json.d,jq '.' $$$$<  1>/dev/null))


#
#  Removes the empty target files of the verify-units-inputs target
#
clean-verify-units-inputs:
	find Units -path "*/.input.*.verified" | xargs rm -f


endif  # if !USING_BMAKE

#
# UNITS Target
#
units: $(CTAGS_TEST)
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	if ! test x$(TRAVIS)$(APPVEYOR)$(CIRCLECI) = x; then	\
		SHOW_DIFF_OUTPUT=--show-diff-output;		\
	fi;							\
	builddir=$$(pwd); \
	mkdir -p $${builddir}/Units && \
	\
	c="$(srcdir)/misc/units run \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		--categories=$(CATEGORIES) \
		--units=$(UNITS) \
		$${VALGRIND} --run-shrink \
		--with-timeout=`expr $(TIMEOUT) '*' 10`\
		$${SHOW_DIFF_OUTPUT}"; \
	 TRAVIS=$(TRAVIS) APPVEYOR=$(APPVEYOR) CIRCLECI=$(CIRCLECI)\
		 $(SHELL) $${c} $(srcdir)/Units $${builddir}/Units

clean-units:
	$(SILENT) echo Cleaning test units
	$(SILENT) builddir=$$(pwd); \
		$(SHELL) $(srcdir)/misc/units clean $${builddir}/Units

#
# Test main part, not parsers
#
tmain: $(CTAGS_TEST)
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	if ! test x$(TRAVIS)$(APPVEYOR)$(CIRCLECI) = x; then	\
		SHOW_DIFF_OUTPUT=--show-diff-output;		\
	fi;							\
	builddir=$$(pwd); \
	mkdir -p $${builddir}/Tmain && \
	\
	c="$(srcdir)/misc/units tmain \
		--ctags=$(CTAGS_TEST) \
		--units=$(UNITS) \
		$${VALGRIND} \
		$${SHOW_DIFF_OUTPUT}"; \
	TRAVIS=$(TRAVIS) APPVEYOR=$(APPVEYOR) CIRCLECI=$(CIRCLECI)\
		$(SHELL) $${c} $(srcdir)/Tmain $${builddir}/Tmain

clean-tmain:
	$(SILENT) echo Cleaning main part tests
	$(SILENT) builddir=$$(pwd); \
		$(SHELL) $(srcdir)/misc/units clean-tmain $${builddir}/Tmain

#
# Test installation
#
tinst:
	$(V_RUN) \
	builddir=$$(pwd); \
	rm -rf $$builddir/$(TINST_ROOT); \
	$(SHELL) $(srcdir)/misc/tinst $(srcdir) $$builddir/$(TINST_ROOT)

#
# Test readtags
#
if USE_READCMD
roundtrip: $(READ_TEST)
	$(V_RUN) \
	builddir=$$(pwd); \
	$(SHELL) $(srcdir)/misc/roundtrip $(READ_TEST) $${builddir}/Units
else
roundtrip:
endif

#
# Checking code in ctags own rules
#
codecheck:
	$(SHELL) misc/src-check

#
# Report coverage (usable only if ctags is built with COVERAGE=1.)
#
run-gcov:
	$(CTAGS_TEST) -o - $$(find ./Units -name 'input.*'| grep -v '.*b/.*') > /dev/null
	gcov $$(find -name '*.gcda')

clean-gcov:
	$(SILENT) echo Cleaning coverage reports
	$(SILENT) rm -f $(ALL_SRCS:.c=.gcda)
	$(SILENT) rm -f $(srcdir)/*.gcov

#
# Cppcheck
#
CPPCHECK_DEFS   = -DHAVE_LIBYAML -DHAVE_LIBXML -DHAVE_COPROC -DHAVE_DECL___ENVIRON
CPPCHECK_UNDEFS = -UDEBUG -UMIO_DEBUG -UCXX_DEBUGGING_ENABLED
CPPCHECK_FLAGS  = --enable=all

cppcheck:
	cppcheck $(CPPCHECK_DEFS) $(CPPCHECK_UNDEFS) $(CPPCHECK_FLAGS) \
		 $$(git  ls-files | grep '^\(parsers\|main\)/.*\.[ch]' )

#
# Spelling
#
dicts: $(CTAGS_TEST)
	${SHELL} misc/make-dictfiles.sh
cspell: $(CTAGS_TEST)
	${SHELL} misc/cspell
