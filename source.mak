# Shared macros

# REPOINFO_HEADS is included from REPOINFO_SRCS
# only when the building environment has ability
# to generate the header file.
# REPOINFO_OBJS is always linked to ctags executable.
REPOINFO_HEADS = main/repoinfo.h
REPOINFO_SRCS  = main/repoinfo.c
REPOINFO_OBJS  = $(REPOINFO_SRCS:.c=.$(OBJEXT))

MAIN_HEADS =			\
	main/args.h		\
	main/ctags.h		\
	main/entry.h		\
	main/field.h		\
	main/flags.h		\
	main/fmt.h		\
	main/general.h		\
	main/get.h		\
	main/htable.h		\
	main/keyword.h		\
	main/kind.h		\
	main/main.h		\
	main/mbcs.h		\
	main/nestlevel.h	\
	main/options.h		\
	main/parse.h		\
	main/parsers.h		\
	main/pcoproc.h		\
	main/ptag.h		\
	main/read.h		\
	main/routines.h		\
	main/selectors.h	\
	main/sort.h		\
	main/strlist.h		\
	main/vstring.h		\
	main/xtag.h

MAIN_SRCS =				\
	main/args.c			\
	main/entry.c			\
	main/field.c			\
	main/flags.c			\
	main/fmt.c			\
	main/get.c			\
	main/htable.c			\
	main/keyword.c			\
	main/kind.c			\
	main/lregex.c			\
	main/lxcmd.c			\
	main/lxpath.c			\
	main/main.c			\
	main/mbcs.c			\
	main/nestlevel.c		\
	main/options.c			\
	main/parse.c			\
	main/pcoproc.c			\
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
	\
	$(NULL)

include makefiles/translator_input.mak
TRANSLATED_SRCS = $(TRANSLATOR_INPUT:.ctags=.c)

PARSER_HEADS =
PARSER_SRCS =				\
	parsers/ada.c			\
	parsers/ant.c			\
	parsers/asm.c			\
	parsers/asp.c			\
	parsers/awk.c			\
	parsers/basic.c			\
	parsers/beta.c			\
	parsers/c.c			\
	parsers/clojure.c		\
	parsers/css.c			\
	parsers/cobol.c			\
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
	parsers/jscript.c		\
	parsers/json.c			\
	parsers/lisp.c			\
	parsers/lua.c			\
	parsers/make.c			\
	parsers/matlab.c		\
	parsers/objc.c			\
	parsers/ocaml.c			\
	parsers/pascal.c		\
	parsers/perl.c			\
	parsers/perl6.c			\
	parsers/php.c			\
	parsers/python.c		\
	parsers/r.c			\
	parsers/rexx.c			\
	parsers/rst.c			\
	parsers/ruby.c			\
	parsers/rust.c			\
	parsers/scheme.c		\
	parsers/sh.c			\
	parsers/slang.c			\
	parsers/sml.c			\
	parsers/sql.c			\
	parsers/tcl.c			\
	parsers/tex.c			\
	parsers/ttcn.c			\
	parsers/verilog.c		\
	parsers/vhdl.c			\
	parsers/vim.c			\
	parsers/windres.c		\
	parsers/yacc.c			\
	\
	$(TRANSLATED_SRCS)		\
	\
	$(NULL)

XML_HEADS =
XML_SRCS = \
	 parsers/maven2.c		\
	 parsers/dbusintrospect.c	\
	 parsers/glade.c		\
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

QUALIFIER_HEAD = dsl/es-lang-c-stdc99.h \
		 dsl/qualifier.h
QUALIFIER_SRCS = dsl/es-lang-c-stdc99.c \
		 dsl/qualifier.c
QUALIFIER_OBJS = $(QUALIFIER_SRCS:.c=.$(OBJEXT))

ALL_OBJS = \
	$(ALL_SRCS:.c=.$(OBJEXT)) \
	$(LIBOBJS)

# vim: ts=8
