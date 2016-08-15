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

MAIN_HEADS =			\
	main/args.h		\
	main/ctags.h		\
	main/dependency.h	\
	main/entry.h		\
	main/error.h		\
	main/field.h		\
	main/flags.h		\
	main/fmt.h		\
	main/gcc-attr.h		\
	main/general.h		\
	main/htable.h		\
	main/keyword.h		\
	main/kind.h		\
	main/lcpp.h		\
	main/main.h		\
	main/mbcs.h		\
	main/mybool.h		\
	main/nestlevel.h	\
	main/options.h		\
	main/output.h		\
	main/parse.h		\
	main/parsers.h		\
	main/pcoproc.h		\
	main/promise.h		\
	main/ptag.h		\
	main/read.h		\
	main/routines.h		\
	main/selectors.h	\
	main/sort.h		\
	main/strlist.h		\
	main/types.h		\
	main/vstring.h		\
	main/xtag.h		\
	\
	$(MIO_HEADS)

MAIN_SRCS =				\
	main/args.c			\
	main/dependency.c		\
	main/entry.c			\
	main/error.c			\
	main/field.c			\
	main/flags.c			\
	main/fmt.c			\
	main/htable.c			\
	main/keyword.c			\
	main/kind.c			\
	main/lcpp.c			\
	main/lregex.c			\
	main/lxcmd.c			\
	main/lxpath.c			\
	main/main.c			\
	main/mbcs.c			\
	main/nestlevel.c		\
	main/options.c			\
	main/output-etags.c		\
	main/output-ctags.c		\
	main/output-json.c		\
	main/output-xref.c		\
	main/parse.c			\
	main/pcoproc.c			\
	main/promise.c			\
	main/ptag.c			\
	main/read.c			\
	main/routines.c			\
	main/selectors.c		\
	main/sort.c			\
	main/strlist.c			\
	main/vstring.c			\
	main/xtag.c			\
	\
	$(REPOINFO_SRCS) \
	$(MIO_SRCS)      \
	\
	$(NULL)

include makefiles/translator_input.mak
TRANSLATED_SRCS = $(TRANSLATOR_INPUT:.ctags=.c)

PARSER_HEADS = \
	parsers/cxx/cxx_debug.h \
	parsers/cxx/cxx_keyword.h \
	parsers/cxx/cxx_parser_internal.h \
	parsers/cxx/cxx_parser.h \
	parsers/cxx/cxx_scope.h \
	parsers/cxx/cxx_tag.h \
	parsers/cxx/cxx_token.h \
	parsers/cxx/cxx_token_chain.h \
	\
	parsers/iniconf.h \
	parsers/make.h \
	\
	$(NULL)
	
PARSER_SRCS =				\
	parsers/ada.c			\
	parsers/ant.c			\
	parsers/asm.c			\
	parsers/asp.c			\
	parsers/automake.c		\
	parsers/awk.c			\
	parsers/basic.c			\
	parsers/beta.c			\
	parsers/c.c			\
	parsers/clojure.c		\
	parsers/css.c			\
	parsers/cobol.c			\
	parsers/cxx/cxx.c		\
	parsers/cxx/cxx_debug.c		\
	parsers/cxx/cxx_keyword.c		\
	parsers/cxx/cxx_parser.c		\
	parsers/cxx/cxx_parser_block.c		\
	parsers/cxx/cxx_parser_function.c		\
	parsers/cxx/cxx_parser_lambda.c		\
	parsers/cxx/cxx_parser_namespace.c		\
	parsers/cxx/cxx_parser_template.c		\
	parsers/cxx/cxx_parser_tokenizer.c		\
	parsers/cxx/cxx_parser_typedef.c		\
	parsers/cxx/cxx_parser_using.c		\
	parsers/cxx/cxx_parser_variable.c		\
	parsers/cxx/cxx_scope.c		\
	parsers/cxx/cxx_tag.c		\
	parsers/cxx/cxx_token.c		\
	parsers/cxx/cxx_token_chain.c		\
	parsers/diff.c			\
	parsers/dosbatch.c		\
	parsers/dts.c			\
	parsers/eiffel.c		\
	parsers/erlang.c		\
	parsers/falcon.c		\
	parsers/flex.c			\
	parsers/fortran.c		\
	parsers/go.c			\
	parsers/html.c			\
	parsers/iniconf.c		\
	parsers/jprop.c			\
	parsers/jscript.c		\
	parsers/json.c			\
	parsers/lisp.c			\
	parsers/lua.c			\
	parsers/make.c			\
	parsers/matlab.c		\
	parsers/myrddin.c		\
	parsers/objc.c			\
	parsers/ocaml.c			\
	parsers/pascal.c		\
	parsers/perl.c			\
	parsers/perl6.c			\
	parsers/php.c			\
	parsers/protobuf.c		\
	parsers/python.c		\
	parsers/pythonloggingconfig.c	\
	parsers/r.c			\
	parsers/rexx.c			\
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
	parsers/tex.c			\
	parsers/ttcn.c			\
	parsers/verilog.c		\
	parsers/vhdl.c			\
	parsers/vim.c			\
	parsers/windres.c		\
	parsers/yacc.c			\
	parsers/yumrepo.c		\
	\
	$(TRANSLATED_SRCS)		\
	\
	$(NULL)

XML_HEADS =
XML_SRCS = \
	 parsers/maven2.c		\
	 parsers/dbusintrospect.c	\
	 parsers/glade.c		\
	 parsers/svg.c			\
	 parsers/plist.c		\
	 parsers/relaxng.c		\
	 parsers/xslt.c			\
	 \
	 $(NULL)

DEBUG_HEADS = main/debug.h
DEBUG_SRCS = main/debug.c

ALL_HEADS = $(MAIN_HEADS) $(PARSER_HEADS) $(DEBUG_HEADS)
ALL_SRCS = $(MAIN_SRCS) $(PARSER_SRCS) $(DEBUG_SRCS)

ENVIRONMENT_HEADS = e_msoft.h
ENVIRONMENT_SRCS =

REGEX_HEADS = gnu_regex/regex.h
REGEX_SRCS = gnu_regex/regex.c
REGEX_OBJS = $(REGEX_SRCS:.c=.$(OBJEXT))

FNMATCH_HEADS = fnmatch/fnmatch.h
FNMATCH_SRCS = fnmatch/fnmatch.c
FNMATCH_OBJS = $(FNMATCH_SRCS:.c=.$(OBJEXT))

QUALIFIER_HEADS = dsl/es-lang-c-stdc99.h \
		 dsl/qualifier.h \
		 \
		 $(MIO_HEADS) \
		 \
		 $(NULL)

QUALIFIER_SRCS = dsl/es-lang-c-stdc99.c \
		 dsl/qualifier.c \
		 \
		 $(MIO_SRCS) \
		 \
		 $(NULL)

QUALIFIER_OBJS = $(QUALIFIER_SRCS:.c=.$(OBJEXT))

ALL_OBJS = \
	$(ALL_SRCS:.c=.$(OBJEXT)) \
	$(LIBOBJS)


READTAGS_SRCS  = \
	       read/readtags.c      \
	       read/readtags-cmd.c  \
	       \
	       $(NULL)
READTAGS_HEADS = read/readtags.h
READTAGS_OBJS  = $(READTAGS_SRCS:.c=.$(OBJEXT))

# vim: ts=8
