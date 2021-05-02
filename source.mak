# Shared macros

# REPOINFO_HEADS is included from REPOINFO_SRCS
# only when the building environment has ability
# to generate the header file.
# REPOINFO_OBJS is always linked to ctags executable.
REPOINFO_HEADS = main/repoinfo.h
REPOINFO_SRCS  = main/repoinfo.c
REPOINFO_OBJS  = $(REPOINFO_SRCS:.c=.$(OBJEXT))

MIO_HEADS = main/mio.h
MIO_SRCS  = main/mio.c

MAIN_PUBLIC_HEADS =		\
	main/dependency.h	\
	main/entry.h		\
	main/field.h		\
	main/gcc-attr.h		\
	main/gvars.h		\
	main/htable.h		\
	main/inline.h		\
	main/keyword.h		\
	main/kind.h		\
	main/lregex.h		\
	main/lxpath.h		\
	main/mbcs.h		\
	main/nestlevel.h	\
	main/numarray.h		\
	main/objpool.h		\
	main/options.h		\
	main/param.h		\
	main/parse.h		\
	main/promise.h		\
	main/ptrarray.h		\
	main/rbtree.h		\
	main/read.h		\
	main/routines.h		\
	main/selectors.h	\
	main/strlist.h		\
	main/subparser.h	\
	main/tokeninfo.h	\
	main/trace.h		\
	main/trashbox.h 	\
	main/types.h		\
	main/unwindi.h  	\
	main/vstring.h		\
	main/xtag.h		\
	\
	$(NULL)

LIB_PRIVATE_HEADS =		\
	main/args_p.h		\
	main/colprint_p.h	\
	main/dependency_p.h	\
	main/entry_p.h		\
	main/error_p.h		\
	main/field_p.h		\
	main/flags_p.h		\
	main/fmt_p.h		\
	main/interactive_p.h	\
	main/keyword_p.h	\
	main/kind_p.h		\
	main/lregex_p.h		\
	main/lxpath_p.h		\
	main/main_p.h		\
	main/mbcs_p.h		\
	main/options_p.h	\
	main/param_p.h		\
	main/parse_p.h		\
	main/parsers_p.h	\
	main/portable-dirent_p.h\
	main/promise_p.h	\
	main/ptag_p.h		\
	main/read_p.h		\
	main/routines_p.h	\
	main/script_p.h		\
	main/sort_p.h		\
	main/stats_p.h		\
	main/subparser_p.h	\
	main/trashbox_p.h	\
	main/writer_p.h		\
	main/xtag_p.h		\
	\
	$(NULL)

LIB_HEADS =			\
	main/ctags.h		\
	main/general.h		\
	\
	$(MAIN_PUBLIC_HEADS)	\
	$(LIB_PRIVATE_HEADS)	\
	\
	$(MIO_HEADS)

LIB_SRCS =			\
	main/args.c			\
	main/colprint.c			\
	main/dependency.c		\
	main/entry.c			\
	main/entry_private.c		\
	main/error.c			\
	main/field.c			\
	main/flags.c			\
	main/fmt.c			\
	main/htable.c			\
	main/keyword.c			\
	main/kind.c			\
	main/lregex.c			\
	main/lxpath.c			\
	main/main.c			\
	main/mbcs.c			\
	main/nestlevel.c		\
	main/numarray.c			\
	main/objpool.c			\
	main/options.c			\
	main/param.c			\
	main/parse.c			\
	main/portable-scandir.c		\
	main/promise.c			\
	main/ptag.c			\
	main/ptrarray.c			\
	main/rbtree.c			\
	main/read.c			\
	main/routines.c			\
	main/script.c			\
	main/seccomp.c			\
	main/selectors.c		\
	main/sort.c			\
	main/stats.c			\
	main/strlist.c			\
	main/trace.c			\
	main/trashbox.c			\
	main/tokeninfo.c		\
	main/unwindi.c			\
	main/vstring.c			\
	main/writer.c			\
	main/writer-etags.c		\
	main/writer-ctags.c		\
	main/writer-json.c		\
	main/writer-xref.c		\
	main/xtag.c			\
	\
	\
	$(TXT2CSTR_SRCS) \
	\
	$(REPOINFO_SRCS) \
	$(MIO_SRCS)      \
	\
	$(NULL)

CMDLINE_HEADS =
CMDLINE_SRCS = \
	main/cmd.c \
	\
	$(NULL)

MINI_GEANY_HEADS =
MINI_GEANY_SRCS = \
	main/mini-geany.c \
	\
	$(NULL)

OPTSCRIPT_SRCS = \
	extra-cmds/optscript-repl.c \
	\
	$(NULL)

include makefiles/optlib2c_input.mak
OPTLIB2C_SRCS = $(OPTLIB2C_INPUT:.ctags=.c)

include makefiles/txt2cstr_input.mak
TXT2CSTR_SRCS = $(TXT2CSTR_INPUT:.ps=.c)

include makefiles/peg_input.mak
PEG_SRCS = $(PEG_INPUT:.peg=.c)
PEG_HEADS = $(PEG_INPUT:.peg=.h)
PEG_EXTRA_HEADS = peg/peg_common.h $(PEG_INPUT:.peg=_pre.h) $(PEG_INPUT:.peg=_post.h)
PEG_OBJS = $(PEG_SRCS:.c=.$(OBJEXT))

PARSER_HEADS = \
	parsers/autoconf.h \
	parsers/cpreprocessor.h \
	\
	parsers/cxx/cxx_debug.h \
	parsers/cxx/cxx_keyword.h \
	parsers/cxx/cxx_parser_internal.h \
	parsers/cxx/cxx_parser.h \
	parsers/cxx/cxx_scope.h \
	parsers/cxx/cxx_subparser.h \
	parsers/cxx/cxx_subparser_internal.h \
	parsers/cxx/cxx_tag.h \
	parsers/cxx/cxx_token.h \
	parsers/cxx/cxx_token_chain.h \
	\
	parsers/iniconf.h \
	parsers/m4.h \
	parsers/make.h \
	parsers/perl.h \
	parsers/r.h \
	parsers/tcl.h \
	parsers/tex.h \
	\
	$(NULL)

PARSER_SRCS =				\
	parsers/abaqus.c		\
	parsers/abc.c			\
	parsers/ada.c			\
	parsers/ant.c			\
	parsers/asciidoc.c		\
	parsers/asm.c			\
	parsers/asp.c			\
	parsers/autoconf.c		\
	parsers/autoit.c		\
	parsers/automake.c		\
	parsers/awk.c			\
	parsers/basic.c			\
	parsers/beta.c			\
	parsers/bibtex.c		\
	parsers/c.c			\
	parsers/clojure.c		\
	parsers/css.c			\
	parsers/cobol.c			\
	parsers/cpreprocessor.c		\
	parsers/cxx/cxx.c		\
	parsers/cxx/cxx_debug.c		\
	parsers/cxx/cxx_debug_type.c	\
	parsers/cxx/cxx_keyword.c		\
	parsers/cxx/cxx_parser.c		\
	parsers/cxx/cxx_parser_block.c		\
	parsers/cxx/cxx_parser_function.c	\
	parsers/cxx/cxx_parser_lambda.c		\
	parsers/cxx/cxx_parser_namespace.c	\
	parsers/cxx/cxx_parser_template.c	\
	parsers/cxx/cxx_parser_tokenizer.c	\
	parsers/cxx/cxx_parser_typedef.c	\
	parsers/cxx/cxx_parser_using.c		\
	parsers/cxx/cxx_parser_variable.c	\
	parsers/cxx/cxx_subparser.c	\
	parsers/cxx/cxx_qtmoc.c		\
	parsers/cxx/cxx_scope.c		\
	parsers/cxx/cxx_tag.c		\
	parsers/cxx/cxx_token.c		\
	parsers/cxx/cxx_token_chain.c	\
	parsers/diff.c			\
	parsers/dosbatch.c		\
	parsers/dtd.c			\
	parsers/dts.c			\
	parsers/eiffel.c		\
	parsers/erlang.c		\
	parsers/falcon.c		\
	parsers/flex.c			\
	parsers/fortran.c		\
	parsers/fypp.c			\
	parsers/go.c			\
	parsers/haskell.c		\
	parsers/haxe.c			\
	parsers/html.c			\
	parsers/iniconf.c		\
	parsers/itcl.c			\
	parsers/jprop.c			\
	parsers/jscript.c		\
	parsers/json.c			\
	parsers/julia.c			\
	parsers/ldscript.c		\
	parsers/lisp.c			\
	parsers/lua.c			\
	parsers/m4.c			\
	parsers/make.c			\
	parsers/matlab.c		\
	parsers/myrddin.c		\
	parsers/nsis.c			\
	parsers/objc.c			\
	parsers/ocaml.c			\
	parsers/pascal.c		\
	parsers/perl.c			\
	parsers/perl-function-parameters.c \
	parsers/perl-moose.c		\
	parsers/perl6.c			\
	parsers/php.c			\
	parsers/powershell.c		\
	parsers/protobuf.c		\
	parsers/python.c		\
	parsers/pythonloggingconfig.c	\
	parsers/r-r6class.c		\
	parsers/r-s4class.c		\
	parsers/r.c			\
	parsers/rexx.c			\
	parsers/robot.c			\
	parsers/rpmspec.c		\
	parsers/rst.c			\
	parsers/ruby.c			\
	parsers/rust.c			\
	parsers/scheme.c		\
	parsers/sh.c			\
	parsers/slang.c			\
	parsers/sml.c			\
	parsers/sql.c			\
	parsers/systemdunit.c		\
	parsers/tcl.c			\
	parsers/tcloo.c			\
	parsers/tex.c			\
	parsers/tex-beamer.c		\
	parsers/ttcn.c			\
	parsers/txt2tags.c		\
	parsers/typescript.c		\
	parsers/verilog.c		\
	parsers/vhdl.c			\
	parsers/vim.c			\
	parsers/windres.c		\
	parsers/yacc.c			\
	parsers/yumrepo.c		\
	\
	$(OPTLIB2C_SRCS)		\
	\
	$(NULL)

XML_HEADS = parsers/xml.h
XML_SRCS = \
	parsers/maven2.c		\
	parsers/dbusintrospect.c	\
	parsers/glade.c			\
	parsers/svg.c			\
	parsers/plist.c			\
	parsers/relaxng.c		\
	parsers/xml.c			\
	parsers/xslt.c			\
	\
	$(NULL)

YAML_HEADS = parsers/yaml.h
YAML_SRCS = \
	parsers/yaml.c		\
	\
	parsers/ansibleplaybook.c	\
	\
	$(NULL)

DEBUG_HEADS = main/debug.h
DEBUG_SRCS = main/debug.c

ALL_LIB_HEADS = $(LIB_HEADS) $(PARSER_HEADS) $(DEBUG_HEADS) $(DSL_HEADS) $(OPTSCRIPT_DSL_HEADS)
ALL_LIB_SRCS  = $(LIB_SRCS) $(PARSER_SRCS) $(DEBUG_SRCS) $(DSL_SRCS) $(OPTSCRIPT_DSL_SRCS)
ALL_HEADS = $(ALL_LIB_HEADS) $(CMDLINE_HEADS)
ALL_SRCS = $(ALL_LIB_SRCS) $(CMDLINE_SRCS)

ENVIRONMENT_HEADS =
ENVIRONMENT_SRCS =

REGEX_HEADS = gnu_regex/regex.h
REGEX_SRCS = gnu_regex/regex.c
REGEX_OBJS = $(REGEX_SRCS:.c=.$(OBJEXT))

FNMATCH_HEADS = fnmatch/fnmatch.h
FNMATCH_SRCS = fnmatch/fnmatch.c
FNMATCH_OBJS = $(FNMATCH_SRCS:.c=.$(OBJEXT))

WIN32_HEADS = main/e_msoft.h
WIN32_SRCS = win32/mkstemp/mkstemp.c
WIN32_OBJS = $(WIN32_SRCS:.c=.$(OBJEXT))

OPTSCRIPT_DSL_HEADS = \
	dsl/es.h \
	dsl/optscript.h \
	\
	$(NULL)

OPTSCRIPT_DSL_SRCS = \
	dsl/es.c \
	dsl/optscript.c \
	\
	$(NULL)

READTAGS_DSL_HEADS = \
	dsl/es.h \
	dsl/dsl.h \
	dsl/qualifier.h \
	dsl/sorter.h \
	\
	$(MIO_HEADS) \
	\
	$(NULL)

READTAGS_DSL_SRCS = \
	dsl/es.c \
	dsl/dsl.c \
	dsl/qualifier.c \
	dsl/sorter.c \
	\
	$(MIO_SRCS) \
	\
	$(NULL)

READTAGS_DSL_OBJS = $(QUALIFIER_SRCS:.c=.$(OBJEXT))

ALL_OBJS = \
	$(ALL_SRCS:.c=.$(OBJEXT)) \
	$(LIBOBJS)

ALL_LIB_OBJS = \
	$(ALL_LIB_SRCS:.c=.$(OBJEXT)) \
	$(LIBOBJS)

READTAGS_SRCS  = \
	libreadtags/readtags.c      \
	extra-cmds/printtags.c  \
	extra-cmds/readtags-cmd.c  \
	\
	$(NULL)
READTAGS_HEADS = \
	       libreadtags/readtags.h \
	       extra-cmds/printtags.h  \
	       \
	       $(NULL)
READTAGS_OBJS  = $(READTAGS_SRCS:.c=.$(OBJEXT))

PACKCC_SRCS = \
	misc/packcc/src/packcc.c \
	\
	$(NULL)

PACKCC_OBJS = $(PACKCC_SRCS:.c=.$(OBJEXT))

OPTSCRIPT_DSL_OBJS = $(OPTSCRIPT_DSL_SRCS:.c=.$(OBJEXT))

OPTSCRIPT_OBJS = $(OPTSCRIPT_SRCS:.c=.$(OBJEXT))

# vim: ts=8
