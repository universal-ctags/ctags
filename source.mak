# Shared macros

HEADERS_DEBUG = main/debug.h

HEADERS_MAIN =			\
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
	main/read.h		\
	main/routines.h		\
	main/selectors.h	\
	main/sort.h		\
	main/strlist.h		\
	main/vstring.h		\
	main/xtag.h

HEADERS_PARSER =

HEADERS_ALL = $(HEADERS_MAIN) $(HEADERS_PARSER) $(HEADERS_DEBUG)

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
	parsers/verilog.c		\
	parsers/vhdl.c			\
	parsers/vim.c			\
	parsers/windres.c		\
	parsers/yacc.c

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
	main/main.c			\
	main/mbcs.c			\
	main/nestlevel.c		\
	main/options.c			\
	main/parse.c			\
	main/pcoproc.c			\
	main/read.c			\
	main/routines.c			\
	main/selectors.c		\
	main/sort.c			\
	main/strlist.c			\
	main/vstring.c			\
	main/xtag.c

DEBUG_SRCS = main/debug.c

ALL_SRCS = $(MAIN_SRCS) $(PARSER_SRCS) $(DEBUG_SRCS)
SOURCES = $(ALL_SRCS) $(HEADERS_MAIN) $(HEADERS_PARSER) $(HEADERS_DEBUG)

HEADERS_ENVIRONMENT = e_msoft.h
ENVIRONMENT_SRCS =

HEADERS_REGEX = gnu_regex/regex.h
REGEX_SRCS = gnu_regex/regex.c

HEADERS_FNMATCH = fnmatch/fnmatch.h
FNMATCH_SRCS = fnmatch/fnmatch.c

OBJECTS = \
	$(ALL_SRCS:.c=.$(OBJEXT)) \
	$(LIBOBJS)

# vim: ts=8
