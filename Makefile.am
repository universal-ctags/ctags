AUTOMAKE_OPTIONS = foreign

include $(top_srcdir)/source.mak
include $(top_srcdir)/testing.mak

bin_PROGRAMS = ctags

if USE_READCMD
bin_PROGRAMS += readtags
readtags_CPPFLAGS = -DREADTAGS_MAIN -I. -I$(srcdir) -I$(srcdir)/main
dist_readtags_SOURCES = readtags.c readtags.h
endif

if !HAVE_FNMATCH
MAIN_SRCS += $(FNMATCH_SRCS)
HEADERS_MAIN += $(HEADERS_FNMATCH)
endif

if !HAVE_REGCOMP
MAIN_SRCS += $(REGEX_SRCS)
HEADERS_MAIN += $(HEADERS_REGEX)
endif

REV != git rev-parse --short HEAD || echo 0
ctags_CPPFLAGS = -I. -I$(srcdir) -I$(srcdir)/main -DCTAGS_COMMIT_ID=0x$(REV)
dist_ctags_SOURCES = $(SOURCES)

dist_man_MANS = ctags.1

PRELOAD_OPTLIB = \
								 data/optlib/coffee.ctags \
								 data/optlib/ctags.ctags \
								 data/optlib/gdbinit.ctags \
								 data/optlib/m4.ctags
DEFAULT_PRELOAD_FILE = default.ctags

optlibdir = $(pkgdatadir)/optlib
dist_optlib_DATA = $(PRELOAD_OPTLIB)

preloaddir = $(pkgdatadir)/preload
dist_preload_DATA = data/optlib/$(DEFAULT_PRELOAD_FILE)

driversdir = $(libexecdir)/drivers
dist_drivers_SCRIPTS = libexec/drivers/coffeetags

install-data-hook:
	for c in $(PRELOAD_OPTLIB); do \
		echo "--options=$$(basename $$c .ctags)" >> $(DESTDIR)$(preloaddir)/$(DEFAULT_PRELOAD_FILE); \
	done

install-exec-hook: install-etags install-lib

install-etags:
if INSTALL_ETAGS
	cd $(DESTDIR)$(bindir) && \
		$(LN_S) ctags$(EXEEXT) etags$(EXEEXT)
endif

install-lib:
if USE_READCMD
if INSTALL_LIB
	$(MKDIR_P) $(DESTDIR)$(libdir) && \
		$(MKDIR_P) $(DESTDIR)$(includedir) && \
		$(INSTALL_DATA) $(readtags_OBJECTS) $(DESTDIR)$(libdir)/readtags.o && \
		$(INSTALL_DATA) readtags.h $(DESTDIR)$(includedir)/readtags.h
endif
endif

uninstall-hook: uninstall-etags uninstall-lib

uninstall-etags:
if INSTALL_ETAGS
	rm $(DESTDIR)$(bindir)/etags$(EXEEXT)
endif

uninstall-lib:
if INSTALL_LIB
	rm $(DESTDIR)$(libdir)/readtags.o && \
		rm $(DESTDIR)$(includedir)/readtags.h
endif


check-local: tmain units
