ACLOCAL_AMFLAGS = -I m4

include $(top_srcdir)/source.mak

PACKCC_FILES = misc/packcc/.gitignore   \
	       misc/packcc/README.md    \
	       misc/packcc/LICENSE      \
	       misc/packcc/src/packcc.c \
	       \
	       $(NULL)

EXTRA_DIST   = README.md autogen.sh \
	       makefiles/list-optlib2c-input.mak misc/optlib2c \
	       .ctags.d/exclusion.ctags .dir-locals.el .editorconfig .gdbinit .gitignore \
	       .indent.pro .uncrustify.cfg \
	       gnu_regex/README.txt gnu_regex/regcomp.c gnu_regex/regexec.c \
	       gnu_regex/regex_internal.c gnu_regex/regex_internal.h \
	       misc/ctags-optlib-mode.el \
	       misc/mk-interactive-request.sh misc/roundtrip misc/tinst \
	       $(PACKCC_FILES) \
	       misc/validators/validator-jq \
	       misc/validators/validator-KNOWN-INVALIDATION \
	       misc/validators/validator-NONE \
	       misc/validators/validator-puppet \
	       $(WIN32_HEADS) $(WIN32_SRCS) mk_mingw.mak mk_mvc.mak \
	       win32/mkstemp/COPYING.MinGW-w64-runtime.txt \
	       win32/ctags.rc win32/ctags.exe.manifest win32/resource.h \
	       win32/ctags_vs2013.sln \
	       win32/ctags_vs2013.vcxproj win32/ctags_vs2013.vcxproj.filters \
	       win32/gen-repoinfo.bat \
	       man/Makefile man/README man/tags.5.rst \
	       makefiles/list-txt2cstr-input.mak misc/txt2cstr \
	       $(PEG_INPUT) $(OPTLIB2C_INPUT) $(TXT2CSTR_INPUT) \
	       docs Tmain Units

CLEANFILES =
MOSTLYCLEANFILES =

clean-local:
	rm -f packcc$(BUILD_EXEEXT)
	@if test "$(top_srcdir)" != "$(top_builddir)"; then \
		rm -rf $(OPTLIB2C_SRCS); \
		rm -rf $(TXT2CSTR_SRCS); \
	fi

bin_PROGRAMS = ctags
noinst_LIBRARIES = libctags.a

noinst_PROGRAMS =
noinst_PROGRAMS += mini-geany

if HAVE_STRNLEN_FOR_BUILD
EXTRA_CPPFLAGS_FOR_BUILD = -DPCC_USE_SYSTEM_STRNLEN
endif

PACKCC = $(top_builddir)/packcc$(BUILD_EXEEXT)

cc4b_verbose = $(cc4b_verbose_@AM_V@)
cc4b_verbose_ = $(cc4b_verbose_@AM_DEFAULT_V@)
cc4b_verbose_0 = @echo CC4BUILD "  $@";
$(PACKCC): $(top_srcdir)/misc/packcc/src/packcc.c
	$(cc4b_verbose)$(CC_FOR_BUILD) $(CFLAGS_FOR_BUILD) $(CPPFLAGS_FOR_BUILD) $(EXTRA_CPPFLAGS_FOR_BUILD) $(LDFLAGS_FOR_BUILD) -o $@ $(top_srcdir)/misc/packcc/src/packcc.c

if USE_READCMD
bin_PROGRAMS+= readtags
readtags_CPPFLAGS = -I. -I$(srcdir) -I$(srcdir)/main -I$(srcdir)/libreadtags
readtags_CFLAGS = $(COVERAGE_CFLAGS) $(WARNING_CFLAGS)
dist_readtags_SOURCES = $(READTAGS_SRCS) $(READTAGS_HEADS)
readtags_CPPFLAGS += -DREADTAGS_DSL -I$(srcdir)/dsl
dist_readtags_SOURCES += $(READTAGS_DSL_SRCS) $(READTAGS_DSL_HEADS)
if !HAVE_REGCOMP
readtags_CPPFLAGS += $(REGCOMP_CPPFLAGS)
dist_readtags_SOURCES += $(REGEX_SRCS) $(REGEX_HEADS)
endif
endif

if !HAVE_FNMATCH
LIB_SRCS += $(FNMATCH_SRCS)
LIB_HEADS += $(FNMATCH_HEADS)
endif

if !HAVE_REGCOMP
LIB_SRCS += $(REGEX_SRCS)
LIB_HEADS += $(REGEX_HEADS)
endif

if HAVE_LIBXML
PARSER_SRCS += $(XML_SRCS)
PARSER_HEADS += $(XML_HEADS)
endif

if HAVE_JANSSON
LIB_SRCS += $(JANSSON_SRCS)
LIB_HEADS += $(JANSSON_HEADS)
endif

if HAVE_LIBYAML
PARSER_SRCS += $(YAML_SRCS)
PARSER_HEADS += $(YAML_HEADS)
endif

PARSER_HEADS += $(PEG_EXTRA_HEADS)

libctags_a_CPPFLAGS = -I. -I$(srcdir) -I$(srcdir)/main -I$(srcdir)/dsl -I$(srcdir)/peg -DHAVE_PACKCC
if ENABLE_DEBUGGING
libctags_a_CPPFLAGS+= $(DEBUG_CPPFLAGS)
endif
if HAVE_STRNLEN
libctags_a_CPPFLAGS += -DUSE_SYSTEM_STRNLEN
endif
libctags_a_CPPFLAGS+= $(FNMATCH_CPPFLAGS)
libctags_a_CPPFLAGS+= $(REGCOMP_CPPFLAGS)
libctags_a_CPPFLAGS+= -DHAVE_REPOINFO_H

libctags_a_CFLAGS   =
libctags_a_CFLAGS  += $(EXTRA_CFLAGS)
libctags_a_CFLAGS  += $(WARNING_CFLAGS)
libctags_a_CFLAGS  += $(COVERAGE_CFLAGS)
libctags_a_CFLAGS  += $(CGCC_CFLAGS)
libctags_a_CFLAGS  += $(LIBXML_CFLAGS)
libctags_a_CFLAGS  += $(JANSSON_CFLAGS)
libctags_a_CFLAGS  += $(LIBYAML_CFLAGS)
libctags_a_CFLAGS  += $(SECCOMP_CFLAGS)

nodist_libctags_a_SOURCES = $(REPOINFO_HEADS) $(PEG_SRCS) $(PEG_HEADS)
BUILT_SOURCES = $(REPOINFO_HEADS)
CLEANFILES += $(REPOINFO_HEADS) $(PEG_SRCS) $(PEG_HEADS)
$(REPOINFO_OBJS): $(REPOINFO_SRCS) $(REPOINFO_HEADS)
repoinfo_verbose = $(repoinfo_verbose_@AM_V@)
repoinfo_verbose_ = $(repoinfo_verbose_@AM_DEFAULT_V@)
repoinfo_verbose_0 = @echo REPOINFO "  $@";
if BUILD_IN_GIT_REPO
GEN_REPOINFO = $(srcdir)/misc/gen-repoinfo
$(REPOINFO_HEADS): FORCE
	$(repoinfo_verbose)$(GEN_REPOINFO) $@
FORCE:
else
$(REPOINFO_HEADS):
	$(repoinfo_verbose)echo > $@
endif

SUFFIXES=

if RUN_OPTLIB2C
optlib2c_verbose = $(optlib2c_verbose_@AM_V@)
optlib2c_verbose_ = $(optlib2c_verbose_@AM_DEFAULT_V@)
optlib2c_verbose_0 = @echo OPTLIB2C "  $@";
OPTLIB2C = $(srcdir)/misc/optlib2c
SUFFIXES += .ctags
.ctags.c:
	$(optlib2c_verbose)$(OPTLIB2C) $< > $@
$(OPTLIB2C_SRCS): $(OPTLIB2C) Makefile
endif

if RUN_TXT2CSTR
txt2cstr_verbose = $(txt2cstr_verbose_@AM_V@)
txt2cstr_verbose_ = $(txt2cstr_verbose_@AM_DEFAULT_V@)
txt2cstr_verbose_0 = @echo TXT2CSTR "  $@";
TXT2CSTR = $(srcdir)/misc/txt2cstr
SUFFIXES += .ps
.ps.c:
	$(txt2cstr_verbose)$(TXT2CSTR) $< > $@
$(TXT2CSTR_SRCS): $(TXT2CSTR) Makefile
endif

packcc_verbose = $(packcc_verbose_@AM_V@)
packcc_verbose_ = $(packcc_verbose_@AM_DEFAULT_V@)
packcc_verbose_0 = @echo PACKCC "    $@";
SUFFIXES += .peg
.peg.c:
	$(packcc_verbose)$(PACKCC) -o $(top_builddir)/peg/$(*F) "$<"
.peg.h:
	$(packcc_verbose)$(PACKCC) -o $(top_builddir)/peg/$(*F) "$<"
# You cannot use $(PACKCC) as a target name here.
$(PEG_SRCS) $(PEG_HEADS): $(PACKCC) Makefile
dist_libctags_a_SOURCES = $(ALL_LIB_HEADS) $(ALL_LIB_SRCS)

ctags_CPPFLAGS = $(libctags_a_CPPFLAGS)
ctags_CFLAGS = $(libctags_a_CFLAGS)
ctags_LDADD  = libctags.a
ctags_LDADD += $(LIBXML_LIBS)
ctags_LDADD += $(JANSSON_LIBS)
ctags_LDADD += $(LIBYAML_LIBS)
ctags_LDADD += $(SECCOMP_LIBS)
ctags_LDADD += $(ICONV_LIBS)
dist_ctags_SOURCES = $(CMDLINE_HEADS) $(CMDLINE_SRCS)

if HOST_MINGW
RES_OBJ = win32/ctags.res.o
ctags_LDADD += $(RES_OBJ)
windres_verbose = $(windres_verbose_@AM_V@)
windres_verbose_ = $(windres_verbose_@AM_DEFAULT_V@)
windres_verbose_0 = @echo WINDRES "   $@";
$(RES_OBJ): win32/ctags.rc win32/ctags.exe.manifest win32/resource.h
	@mkdir -p $(builddir)/win32
	$(windres_verbose)$(WINDRES) -o $@ -O coff $<
MOSTLYCLEANFILES += $(RES_OBJ)
endif

mini_geany_CPPFLAGS = $(libctags_a_CPPFLAGS)
mini_geany_CFLAGS = $(libctags_a_CFLAGS)
mini_geany_LDADD  = libctags.a
mini_geany_LDADD += $(LIBXML_LIBS)
mini_geany_LDADD += $(JANSSON_LIBS)
mini_geany_LDADD += $(LIBYAML_LIBS)
mini_geany_LDADD += $(SECCOMP_LIBS)
mini_geany_LDADD += $(ICONV_LIBS)
mini_geany_SOURCES = $(MINI_GEANY_HEADS) $(MINI_GEANY_SRCS)

bin_PROGRAMS += optscript
optscript_CPPFLAGS = $(libctags_a_CPPFLAGS)
optscript_CFLAGS = $(libctags_a_CFLAGS)
optscript_LDADD  = libctags.a
# TODO: remove these dependencies from optscript command.
optscript_LDADD += $(LIBXML_LIBS)
optscript_LDADD += $(JANSSON_LIBS)
optscript_LDADD += $(LIBYAML_LIBS)
optscript_LDADD += $(SECCOMP_LIBS)
optscript_LDADD += $(ICONV_LIBS)
optscript_SOURCES = $(OPTSCRIPT_SRCS)

if INSTALL_ETAGS
install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
		$(LN_S) ctags$(EXEEXT) etags$(EXEEXT)

uninstall-hook:
	rm $(DESTDIR)$(bindir)/etags$(EXEEXT)
endif

dist-hook:
	rm -rf $(distdir)/docs/_build
	find $(distdir)/Tmain $(distdir)/Units \( \
		-name '*-actual.txt' -o -name '*-diff.txt' -o \
		-name '*.TMP' -o -name '*.tmp' -o -name '*~' -o \
		-name 'dist.m4' -o -name 'Makefile.am' -o \
		-name 'Makefile.in' -o -name 'Makefile' -o \
		-name 'BUNDLES' \) -exec rm -f {} +

include $(top_srcdir)/makefiles/testing.mak

include $(top_srcdir)/makefiles/help.mak

if HAVE_RST2MAN
# TODO: the list of man pages should be shared with man/Makefile.
man_MANS = \
	man/ctags.1 \
	man/readtags.1 \
	\
	man/tags.5 \
	\
	man/ctags-client-tools.7 \
	man/ctags-incompatibilities.7 \
	man/ctags-optlib.7 \
	man/ctags-faq.7 \
	\
	man/ctags-lang-iPythonCell.7 \
	man/ctags-lang-julia.7 \
	man/ctags-lang-python.7 \
	man/ctags-lang-verilog.7 \
	man/ctags-lang-inko.7 \
	man/ctags-lang-r.7 \
	\
	$(NULL)
rst2man_verbose = $(rst2man_verbose_@AM_V@)
rst2man_verbose_ = $(rst2man_verbose_@AM_DEFAULT_V@)
rst2man_verbose_0 = @echo RST2MAN "   $@";
SUFFIXES += .1.rst .1 .5.rst .5 .7.rst .7
.1.rst.1:
	$(rst2man_verbose)$(RST2MAN) $(RST2MAN_OPTIONS) $< $@
.5.rst.5:
	$(rst2man_verbose)$(RST2MAN) $(RST2MAN_OPTIONS) $< $@
.7.rst.7:
	$(rst2man_verbose)$(RST2MAN) $(RST2MAN_OPTIONS) $< $@
CLEANFILES += $(man_MANS)
endif
