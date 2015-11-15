# Shared macros

DEBUG_HEADS = main/debug.h

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
	main/read.h		\
	main/routines.h		\
	main/selectors.h	\
	main/sort.h		\
	main/strlist.h		\
	main/vstring.h		\
	main/xtag.h

PARSER_HEADS =

PARSER_SOURCES =			\
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

MAIN_SOURCES =				\
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

DEBUG_SOURCES = main/debug.c

SOURCES =	\
		$(MAIN_SOURCES) $(MAIN_HEADS)		\
		$(PARSER_SOURCES) $(PARSER_HEADS)	\
		$(DEBUG_SOURCES) $(DEBUG_HEADS)

OBJECTS = \
	$(SOURCES:.c=.$(OBJEXT)) \
	$(LIBOBJS)

# vim: ts=8
