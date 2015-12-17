include $(top_srcdir)/source.mak

# Test cases are added to EXTRA_DIST in makefiles/test-cases.mak
EXTRA_DIST   = $(NULL)

bin_PROGRAMS = ctags

if USE_READCMD
bin_PROGRAMS+= readtags
readtags_CPPFLAGS = -DREADTAGS_MAIN -I. -I$(srcdir) -I$(srcdir)/main
dist_readtags_SOURCES = readtags.c readtags.h
endif

if !HAVE_FNMATCH
MAIN_SRCS += $(FNMATCH_SRCS)
MAIN_HEADS += $(FNMATCH_HEADS)
endif

if !HAVE_REGCOMP
MAIN_SRCS += $(REGEX_SRCS)
MAIN_HEADS += $(REGEX_HEADS)
endif

ctags_CPPFLAGS = -I. -I$(srcdir) -I$(srcdir)/main
ctags_CPPFLAGS+= $(DEBUG_CPPFLAGS)
ctags_CPPFLAGS+= $(FNMATCH_CPPFLAGS)
ctags_CPPFLAGS+= $(REGCOMP_CPPFLAGS)
ctags_CPPFLAGS+= -DHAVE_REPOINFO_H

ctags_CFLAGS   = $(NULL)
ctags_CFLAGS  += $(EXTRA_CFLAGS)
ctags_CFLAGS  += $(WARNING_CFLAGS)
ctags_CFLAGS  += $(COVERAGE_CFLAGS)
ctags_CFLAGS  += $(CGCC_CFLAGS)

nodist_ctags_SOURCES = $(REPOINFO_HEADS)
BUILT_SOURCES = $(REPOINFO_HEADS)
CLEANFILES = $(REPOINFO_HEADS)
$(REPOINFO_OBJS): $(REPOINFO_SRCS) $(REPOINFO_HEADS)
if BUILD_IN_GIT_REPO
GEN_REPOINFO = $(srcdir)/misc/gen-repoinfo
$(REPOINFO_HEADS): $(GEN_REPOINFO) $(srcdir)/.git/*
	$(GEN_REPOINFO) > $@
else
$(REPOINFO_HEADS):
	echo 0 > $@
endif

OPTLIB2C = $(srcdir)/misc/optlib2c
.ctags.c: $(OPTLIB2C)
	$(OPTLIB2C) $< > $@
dist_ctags_SOURCES = $(ALL_HEADS) $(ALL_SRCS)

man_MANS = ctags.1

if ENABLE_XCMD
driversdir = $(pkglibexecdir)/drivers
dist_drivers_SCRIPTS = libexec/drivers/coffeetags
endif

if INSTALL_ETAGS
install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
		$(LN_S) ctags$(EXEEXT) etags$(EXEEXT)

uninstall-hook:
	rm $(DESTDIR)$(bindir)/etags$(EXEEXT)
endif

include $(top_srcdir)/makefiles/testing.mak
include $(top_srcdir)/makefiles/test-cases.mak

include $(top_srcdir)/makefiles/help.mak

