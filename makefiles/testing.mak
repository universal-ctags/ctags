# -*- makefile -*-
.PHONY: check units fuzz noise tmain tinst tlib man-test clean-units clean-tlib clean-tmain clean-gcov clean-man-test run-gcov codecheck cppcheck dicts validate-input check-genfile

EXTRA_DIST += misc/units misc/units.py misc/man-test.py
EXTRA_DIST += misc/tlib misc/mini-geany.expected
MAN_TEST_TMPDIR = ManTest

check: tmain units tlib man-test check-genfile

# We may use CLEANFILES, DISTCLEANFILES, or etc.
# clean-tlib and clean-gcov are not included
clean-local: clean-units clean-tmain clean-man-test

CTAGS_TEST = ./ctags$(EXEEXT)
READTAGS_TEST = ./readtags$(EXEEXT)
MINI_GEANY_TEST = ./mini-geany$(EXEEXT)
OPTSCRIPT_TEST = ./optscript$(EXEEXT)

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
		--with-timeout=`expr $(TIMEOUT) '*' 10`"; \
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
	if ! test x$(CI) = x; then	\
		SHOW_DIFF_OUTPUT=--show-diff-output;		\
	fi;							\
	builddir=$$(pwd); \
	if ! test x$(PYTHON) = x; then	\
		PROG=$(PYTHON);		\
		SCRIPT=$(srcdir)/misc/units.py;	\
		if type cygpath > /dev/null 2>&1; then	\
			builddir=$$(cygpath -m "$$(pwd)");	\
			if ! test x$(SHELL) = x; then	\
				SHELL_OPT=--shell=$$(cygpath -m $(SHELL));	\
			fi;	\
		else	\
			if ! test x$(SHELL) = x; then	\
				SHELL_OPT=--shell=$(SHELL);	\
			fi;	\
		fi;	\
	else	\
		PROG=$(SHELL);		\
		SCRIPT=$(srcdir)/misc/units;	\
	fi;	\
	mkdir -p $${builddir}/Units && \
	\
	c="$${SCRIPT} run \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		--categories=$(CATEGORIES) \
		--units=$(UNITS) \
		--with-pretense-map=$(PMAP) \
		$${VALGRIND} --run-shrink \
		--with-timeout=`expr $(TIMEOUT) '*' 10`\
		$${SHELL_OPT} \
		$${SHOW_DIFF_OUTPUT}"; \
		 $${PROG} $${c} $(srcdir)/Units $${builddir}/Units

clean-units:
	$(SILENT) echo Cleaning test units
	$(SILENT) if test -d $$(pwd)/Units; then \
		$(SHELL) $(srcdir)/misc/units clean $$(pwd)/Units; \
	fi

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
		$(SHELL) $${c} $(srcdir)/Units $(srcdir)/misc/validators
#
# Test main part, not parsers
#
tmain: $(CTAGS_TEST) $(READTAGS_TEST) $(OPTSCRIPT_TEST)
	$(V_RUN) \
	if test -n "$${ZSH_VERSION+set}"; then set -o SH_WORD_SPLIT; fi; \
	if test x$(VG) = x1; then		\
		VALGRIND=--with-valgrind;	\
	fi;					\
	if ! test x$(CI) = x; then	\
		SHOW_DIFF_OUTPUT=--show-diff-output;		\
	fi;							\
	builddir=$$(pwd); \
	if ! test x$(PYTHON) = x; then	\
		PROG=$(PYTHON);		\
		SCRIPT=$(srcdir)/misc/units.py;	\
		if type cygpath > /dev/null 2>&1; then	\
			builddir=$$(cygpath -m "$$(pwd)");	\
			if ! test x$(SHELL) = x; then	\
				SHELL_OPT=--shell=$$(cygpath -m $(SHELL));	\
			fi;	\
		else	\
			if ! test x$(SHELL) = x; then	\
				SHELL_OPT=--shell=$(SHELL);	\
			fi;	\
		fi;	\
	else	\
		PROG=$(SHELL);		\
		SCRIPT=$(srcdir)/misc/units;	\
	fi;	\
	mkdir -p $${builddir}/Tmain && \
	\
	c="$${SCRIPT} tmain \
		--ctags=$(CTAGS_TEST) \
		--units=$(UNITS) \
		$${VALGRIND} \
		$${SHELL_OPT} \
		$${SHOW_DIFF_OUTPUT}"; \
		$${PROG} $${c} $(srcdir)/Tmain $${builddir}/Tmain

clean-tmain:
	$(SILENT) echo Cleaning main part tests
	$(SILENT) if test -d $$(pwd)/Tmain; then \
		$(SHELL) $(srcdir)/misc/units clean-tmain $$(pwd)/Tmain; \
	fi

tlib: $(MINI_GEANY_TEST)
	$(V_RUN) \
	builddir=$$(pwd); \
	mkdir -p $${builddir}/misc; \
	if test -s '$(MINI_GEANY_TEST)'; then \
		if $(SHELL) $(srcdir)/misc/tlib $(MINI_GEANY_TEST) \
			$(srcdir)/misc/mini-geany.expected \
			$${builddir}/misc/mini-geany.actual \
			$(VG); then \
			echo 'mini-geany: OK'; \
		else \
			echo 'mini-geany: FAILED'; \
		fi; \
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
roundtrip: $(READTAGS_TEST)
	$(V_RUN) \
	if ! test x$(CI) = x; then	\
		ROUNDTRIP_FLAGS=--minitrip;			\
	fi;							\
	builddir=$$(pwd); \
	$(SHELL) $(srcdir)/misc/roundtrip $(READTAGS_TEST) $${builddir}/Units $${ROUNDTRIP_FLAGS}
else
roundtrip:
endif

#
# Checking code in ctags own rules
#
codecheck: $(CTAGS_TEST)
	$(V_RUN) $(SHELL) misc/src-check

#
# Report coverage (usable only if ctags is built with "configure --enable-coverage-gcov".)
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
# Testing examples in per-language man pages
#
man-test: $(CTAGS_TEST)
	$(V_RUN) \
	$(PYTHON) $(srcdir)/misc/man-test.py $(MAN_TEST_TMPDIR) $(CTAGS_TEST) $(srcdir)/man/ctags-lang-*.7.rst.in

clean-man-test:
	rm -rf $(MAN_TEST_TMPDIR)

# check if generated files are committed.
#   Note: "make -B" cannot be used here, since it reruns automake
chkgen_verbose = $(chkgen_verbose_@AM_V@)
chkgen_verbose_ = $(chkgen_verbose_@AM_DEFAULT_V@)
chkgen_verbose_0 = @echo CHKGEN "    $@";
check-genfile:
# OPTLIB2C_SRCS : committed for win32 build
	$(chkgen_verbose)rm -f $(OPTLIB2C_SRCS)
	$(chkgen_verbose)$(MAKE) $(OPTLIB2C_SRCS)
	$(chkgen_verbose)if ! git diff --exit-code $(OPTLIB2C_DIR); then \
		echo "Files under $(OPTLIB2C_DIR) are not up to date." ; \
		echo "If you change $(OPTLIB2C_DIR)/foo.ctags, don't forget to add $(OPTLIB2C_DIR)/foo.c to your commit." ; \
		exit 1 ; \
	else \
		echo "Files under $(OPTLIB2C_DIR) are up to date." ; \
	fi
# TXT2CSTR_SRCS : committed for win32 build
	$(chkgen_verbose)rm -f $(TXT2CSTR_SRCS)
	$(chkgen_verbose)$(MAKE) $(TXT2CSTR_SRCS)
	$(chkgen_verbose)if ! git diff --exit-code $(TXT2CSTR_DIR); then \
		echo "Files under $(TXT2CSTR_DIR) are not up to date." ; \
		echo "If you change $(TXT2CSTR_DIR)/foo.ps, don't forget to add $(TXT2CSTR_DIR)/foo.c to your commit." ; \
		exit 1 ; \
	else \
		echo "Files under $(TXT2CSTR_DIR) are up to date." ; \
	fi
if HAVE_RST2MAN
# man/*.in : committed for man pages to be genrated without rst2man
#   make clean-docs remove both man/*.in and docs/man/*.rst
	$(chkgen_verbose)$(MAKE) -C man clean-docs
	$(chkgen_verbose)$(MAKE) -C man man-in
	$(chkgen_verbose)if ! git diff --exit-code -- man; then \
		echo "Files under man/ are not up to date." ; \
		echo "Please execute 'make -C man man-in' and commit them." ; \
		exit 1 ; \
	else \
		echo "Files under man are up to date." ; \
	fi
# docs/man/*.rst : committed for Read the Docs
	$(chkgen_verbose)$(MAKE) -C man update-docs
	$(chkgen_verbose)if ! git diff --exit-code -- docs/man; then \
		echo "Files under docs/man/ are not up to date." ; \
		echo "Please execute 'make -C man update-docs' and commit them." ; \
		exit 1 ; \
	else \
		echo "Files under docs/man are up to date." ; \
	fi
endif
# win32/ctags_vs2013.vcxproj* : committed for win32 build without POSIX tools
#   regenerate files w/o out-of-source build and w/ GNU make
	$(chkgen_verbose)if test "$(top_srcdir)" = "$(top_builddir)" \
		&& ($(MAKE) --version) 2>/dev/null | grep -q GNU ; then \
		$(MAKE) -BC win32 ; \
	fi
	$(chkgen_verbose)if ! git diff --exit-code -- win32; then \
		echo "Files under win32/ are not up to date." ; \
		echo "Please execute 'make -BC win32' and commit them." ; \
		exit 1 ; \
	else \
		echo "Files under win32 are up to date." ; \
	fi
