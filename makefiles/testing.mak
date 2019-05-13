# -*- makefile -*-
.PHONY: check units fuzz noise tmain tinst tlib clean-units clean-tlib clean-tmain clean-gcov run-gcov codecheck cppcheck dicts cspell validate-input

EXTRA_DIST += misc/units
EXTRA_DIST += misc/tlib misc/mini-geany.expected

DIST_SUBDIRS = Tmain Units

check: tmain units tlib

clean-local: clean-units clean-tmain

CTAGS_TEST = ./ctags$(EXEEXT)
READ_TEST = ./readtags$(EXEEXT)
MINI_GEANY_TEST = ./mini-geany$(EXEEXT)

if HAVE_TIMEOUT
TIMEOUT = 1
else
TIMEOUT = 0
endif

LANGUAGES=
CATEGORIES=
UNITS=
PMAP=

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
		--with-pretense-map=$(PMAP) \
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
# VALIDATE-INPUT Target
#
validate-input:
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	if test -n "$(VALIDATORS)"; then	\
		VALIDATORS="--validators=$(VALIDATORS)"; \
	fi; \
	c="$(srcdir)/misc/units validate-input $${VALIDATORS}"; \
	TRAVIS=$(TRAVIS) APPVEYOR=$(APPVEYOR) CIRCLECI=$(CIRCLECI)\
		$(SHELL) $${c} $(srcdir)/Units $(srcdir)/misc/validators
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

tlib: $(MINI_GEANY_TEST)
	$(V_RUN) \
	builddir=$$(pwd); \
	mkdir -p $${builddir}/misc; \
	if test -s '$(MINI_GEANY_TEST)'; then \
		$(SHELL) $(srcdir)/misc/tlib $(MINI_GEANY_TEST) \
			$(srcdir)/misc/mini-geany.expected \
			$${builddir}/misc/mini-geany.actual \
			$(VG); \
		echo 'mini-geany: OK'; \
	else \
		echo 'mini-geany: SKIP'; \
	fi
clean-tlib:
	$(SILENT) echo Cleaning libctags part tests
	$(SILENT) builddir=$$(pwd); \
		rm -f $${builddir}/misc/mini-geany.actual

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
