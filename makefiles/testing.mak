.PHONY: check units fuzz noise tmain tinst clean-units clean-tmain clean-gcov run-gcov codecheck

check: tmain units

ifdef VG
VALGRIND=--with-valgrind
endif
ifdef TRAVIS
SHOW_DIFF_OUTPUT=--show-diff-output
endif
ifdef APPVEYOR
SHOW_DIFF_OUTPUT=--show-diff-output
endif

CTAGS_TEST = ./ctags$(EXEEXT)
READ_TEST = ./$(READ_CMD)
TIMEOUT=
LANGUAGES=
CATEGORIES=
UNITS=

#
# FUZZ Target
#
# SHELL must be dash or bash.
#
fuzz: TIMEOUT := $(shell timeout --version > /dev/null 2>&1 && echo 1 || echo 0)
fuzz: $(CTAGS_TEST)
	@ \
	c="$(srcdir)/misc/units fuzz \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		--datadir=$(srcdir)/data \
		--libexecdir=$(srcdir)/libexec \
		$(VALGRIND) --run-shrink \
		--with-timeout=$(TIMEOUT)"; \
	$(SHELL) $${c} $(srcdir)/Units

#
# NOISE Target
#
noise: $(CTAGS_TEST)
	@ \
	c="$(srcdir)/misc/units noise \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		--datadir=$(srcdir)/data \
		--libexecdir=$(srcdir)/libexec \
		$(VALGRIND) --run-shrink \
		--with-timeout=$(TIMEOUT)"; \
	$(SHELL) $${c} $(srcdir)/Units

#
# CHOP Target
#
chop: $(CTAGS_TEST)
	@ \
	c="$(srcdir)/misc/units chop \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		--datadir=$(srcdir)/data \
		--libexecdir=$(srcdir)/libexec \
		$(VALGRIND) --run-shrink \
		--with-timeout=$(TIMEOUT)"; \
	$(SHELL) $${c} $(srcdir)/Units

#
# UNITS Target
#
units: TIMEOUT := $(shell timeout --version > /dev/null 2>&1 && echo 5 || echo 0)
units: $(CTAGS_TEST)
	@ \
	builddir=$$(pwd); \
	mkdir -p $${builddir}/Units && \
	\
	c="$(srcdir)/misc/units run \
		--ctags=$(CTAGS_TEST) \
		--languages=$(LANGUAGES) \
		--categories=$(CATEGORIES) \
		--units=$(UNITS) \
		--datadir=$(srcdir)/data \
		--libexecdir=$(srcdir)/libexec \
		$(VALGRIND) --run-shrink \
		--with-timeout=$(TIMEOUT) \
		$(SHOW_DIFF_OUTPUT)"; \
	 $(SHELL) $${c} $(srcdir)/Units $${builddir}/Units

clean-units:
	$(SILENT) echo Cleaning test units
	$(SILENT) builddir=$$(pwd); \
		$(SHELL) $(srcdir)/misc/units clean $${builddir}/Units

#
# Test main part, not parsers
#
tmain: $(CTAGS_TEST)
	@ \
	\
	builddir=$$(pwd); \
	mkdir -p $${builddir}/Tmain && \
	\
	c="$(srcdir)/misc/units tmain \
		--ctags=$(CTAGS_TEST) \
		--datadir=$(srcdir)/data \
		--libexecdir=$(srcdir)/libexec \
		$(VALGRIND) \
		$(SHOW_DIFF_OUTPUT)"; \
	 $(SHELL) $${c} $(srcdir)/Tmain $${builddir}/Tmain

clean-tmain:
	$(SILENT) echo Cleaning main part tests
	$(SILENT) builddir=$$(pwd); \
		$(SHELL) $(srcdir)/misc/units clean-tmain $${builddir}/Tmain

#
# Test installation
#
tinst:
	@ \
	\
	builddir=$$(pwd); \
	rm -rf $$builddir/$(TINST_ROOT); \
	\
	$(SHELL) $(srcdir)/misc/tinst $(srcdir) $$builddir/$(TINST_ROOT)

#
# Test readtags
#
roundtrip: $(READ_TEST)
	@ \
	\
	builddir=$$(pwd); \
	\
	$(SHELL) $(srcdir)/misc/roundtrip $(READ_TEST) $${builddir}/Units

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
	$(SILENT) rm -f $(SOURCES:.c=.gcda)
	$(SILENT) rm -f $(srcdir)/*.gcov
