include $(top_srcdir)/source.mak

# Test cases are added to EXTRA_DIST in makefiles/test-cases.mak
EXTRA_DIST   = README.md autogen.sh \
	       .ctags .dir-locals.el .editorconfig .gdbinit .gitignore \
	       .indent.pro .uncrustify.cfg \
	       gnu_regex/README.txt gnu_regex/regcomp.c gnu_regex/regexec.c \
	       gnu_regex/regex_internal.c gnu_regex/regex_internal.h \
	       $(WIN32_HEADS) $(WIN32_SRCS) mk_mingw.mak mk_mvc.mak \
	       win32/mkstemp/COPYING.MinGW-w64-runtime.txt \
	       win32/ctags_vs2013.sln \
	       win32/ctags_vs2013.vcxproj win32/ctags_vs2013.vcxproj.filters \
	       win32/gen-repoinfo.bat

bin_PROGRAMS = ctags

if USE_READCMD
bin_PROGRAMS+= readtags
readtags_CPPFLAGS = -I. -I$(srcdir) -I$(srcdir)/main -I$(srcdir)/read
readtags_CFLAGS = $(COVERAGE_CFLAGS)
dist_readtags_SOURCES = $(READTAGS_SRCS) $(READTAGS_HEADS)
readtags_CPPFLAGS += -DQUALIFIER -I$(srcdir)/dsl
dist_readtags_SOURCES += $(QUALIFIER_SRCS) $(QUALIFIER_HEADS)
endif

if !HAVE_FNMATCH
MAIN_SRCS += $(FNMATCH_SRCS)
MAIN_HEADS += $(FNMATCH_HEADS)
endif

if !HAVE_REGCOMP
MAIN_SRCS += $(REGEX_SRCS)
MAIN_HEADS += $(REGEX_HEADS)
endif

if HAVE_LIBXML
PARSER_SRCS += $(XML_SRCS)
PARSER_HEADS += $(XML_HEADS)
endif

if HAVE_JANSSON
MAIN_SRCS += $(JANSSON_SRCS)
MAIN_HEADS += $(JANSSON_HEADS)
endif

if HAVE_LIBYAML
PARSER_SRCS += $(YAML_SRCS)
PARSER_HEADS += $(YAML_HEADS)
endif

ctags_CPPFLAGS = -I. -I$(srcdir) -I$(srcdir)/main
if ENABLE_DEBUGGING
ctags_CPPFLAGS+= $(DEBUG_CPPFLAGS)
endif
ctags_CPPFLAGS+= $(FNMATCH_CPPFLAGS)
ctags_CPPFLAGS+= $(REGCOMP_CPPFLAGS)
ctags_CPPFLAGS+= -DHAVE_REPOINFO_H

ctags_CFLAGS   =
ctags_CFLAGS  += $(EXTRA_CFLAGS)
ctags_CFLAGS  += $(WARNING_CFLAGS)
ctags_CFLAGS  += $(COVERAGE_CFLAGS)
ctags_CFLAGS  += $(CGCC_CFLAGS)
ctags_CFLAGS  += $(LIBXML_CFLAGS)
ctags_CFLAGS  += $(JANSSON_CFLAGS)
ctags_CFLAGS  += $(LIBYAML_CFLAGS)
ctags_CFLAGS  += $(ASPELL_CFLAGS)
ctags_CFLAGS  += $(SECCOMP_CFLAGS)

ctags_LDADD  =
ctags_LDADD += $(LIBXML_LIBS)
ctags_LDADD += $(JANSSON_LIBS)
ctags_LDADD += $(LIBYAML_LIBS)
ctags_LDADD += $(SECCOMP_LIBS)
ctags_LDADD += $(ASPELL_LIBS)
ctags_LDADD += $(ICONV_LIBS)

nodist_ctags_SOURCES = $(REPOINFO_HEADS)
BUILT_SOURCES = $(REPOINFO_HEADS)
CLEANFILES = $(REPOINFO_HEADS)
$(REPOINFO_OBJS): $(REPOINFO_SRCS) $(REPOINFO_HEADS)
if BUILD_IN_GIT_REPO
GEN_REPOINFO = $(srcdir)/misc/gen-repoinfo
$(REPOINFO_HEADS): FORCE
	$(GEN_REPOINFO) $@
FORCE:
else
$(REPOINFO_HEADS):
	echo > $@
endif

if RUN_OPTLIB2C
optlib2c_verbose = $(optlib2c_verbose_@AM_V@)
optlib2c_verbose_ = $(optlib2c_verbose_@AM_DEFAULT_V@)
optlib2c_verbose_0 = @echo OPTLIB2C "  $@";
OPTLIB2C = $(srcdir)/misc/optlib2c
%.c: %.ctags $(OPTLIB2C) Makefile
	$(optlib2c_verbose)$(OPTLIB2C) $< > $@
endif
dist_ctags_SOURCES = $(ALL_HEADS) $(ALL_SRCS)

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

if HAVE_RST2MAN
man_MANS = man/ctags.1 man/ctags-incompatibilities.7 man/ctags-optlib.7
rst2man_verbose = $(rst2man_verbose_@AM_V@)
rst2man_verbose_ = $(rst2man_verbose_@AM_DEFAULT_V@)
rst2man_verbose_0 = @echo RST2MAN "  $@";
SUFFIXES = .1.rst .1 .7.rst .7
# See man/Makefile
RST2MAN_OPTIONS=--syntax-highlight=none
.1.rst.1:
	$(rst2man_verbose)$(RST2MAN) $(RST2MAN_OPTIONS) $< $@
.7.rst.7:
	$(rst2man_verbose)$(RST2MAN) $(RST2MAN_OPTIONS) $< $@
endif
