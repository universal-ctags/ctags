# Shared macros

PARSER_DIR=parsers
MAIN_DIR=main

DEBUG_HEADERS = $(MAIN_DIR)/debug.h

MAIN_HEADERS =				\
	$(MAIN_DIR)/args.h		\
	$(MAIN_DIR)/ctags.h		\
	$(MAIN_DIR)/entry.h		\
	$(MAIN_DIR)/field.h		\
	$(MAIN_DIR)/flags.h		\
	$(MAIN_DIR)/fmt.h		\
	$(MAIN_DIR)/general.h		\
	$(MAIN_DIR)/get.h		\
	$(MAIN_DIR)/htable.h		\
	$(MAIN_DIR)/keyword.h		\
	$(MAIN_DIR)/kind.h		\
	$(MAIN_DIR)/main.h		\
	$(MAIN_DIR)/mbcs.h		\
	$(MAIN_DIR)/nestlevel.h		\
	$(MAIN_DIR)/options.h		\
	$(MAIN_DIR)/parse.h		\
	$(MAIN_DIR)/parsers.h		\
	$(MAIN_DIR)/pcoproc.h		\
	$(MAIN_DIR)/read.h		\
	$(MAIN_DIR)/routines.h		\
	$(MAIN_DIR)/selectors.h		\
	$(MAIN_DIR)/sort.h		\
	$(MAIN_DIR)/strlist.h		\
	$(MAIN_DIR)/vstring.h

PARSER_HEADERS =

HEADERS = $(MAIN_HEADERS) $(PARSER_HEADERS) $(DEBUG_HEADERS)

PARSER_SOURCES =				\
	$(PARSER_DIR)/ada.c			\
	$(PARSER_DIR)/ant.c			\
	$(PARSER_DIR)/asm.c			\
	$(PARSER_DIR)/asp.c			\
	$(PARSER_DIR)/awk.c			\
	$(PARSER_DIR)/basic.c			\
	$(PARSER_DIR)/beta.c			\
	$(PARSER_DIR)/c.c			\
	$(PARSER_DIR)/clojure.c			\
	$(PARSER_DIR)/css.c			\
	$(PARSER_DIR)/cobol.c			\
	$(PARSER_DIR)/diff.c			\
	$(PARSER_DIR)/dosbatch.c		\
	$(PARSER_DIR)/dts.c			\
	$(PARSER_DIR)/eiffel.c			\
	$(PARSER_DIR)/erlang.c			\
	$(PARSER_DIR)/falcon.c			\
	$(PARSER_DIR)/flex.c			\
	$(PARSER_DIR)/fortran.c			\
	$(PARSER_DIR)/go.c			\
	$(PARSER_DIR)/html.c			\
	$(PARSER_DIR)/jscript.c			\
	$(PARSER_DIR)/json.c			\
	$(PARSER_DIR)/lisp.c			\
	$(PARSER_DIR)/lua.c			\
	$(PARSER_DIR)/make.c			\
	$(PARSER_DIR)/matlab.c			\
	$(PARSER_DIR)/objc.c			\
	$(PARSER_DIR)/ocaml.c			\
	$(PARSER_DIR)/pascal.c			\
	$(PARSER_DIR)/perl.c			\
	$(PARSER_DIR)/perl6.c			\
	$(PARSER_DIR)/php.c			\
	$(PARSER_DIR)/python.c			\
	$(PARSER_DIR)/r.c			\
	$(PARSER_DIR)/rexx.c			\
	$(PARSER_DIR)/rst.c			\
	$(PARSER_DIR)/ruby.c			\
	$(PARSER_DIR)/rust.c			\
	$(PARSER_DIR)/scheme.c			\
	$(PARSER_DIR)/sh.c			\
	$(PARSER_DIR)/slang.c			\
	$(PARSER_DIR)/sml.c			\
	$(PARSER_DIR)/sql.c			\
	$(PARSER_DIR)/tcl.c			\
	$(PARSER_DIR)/tex.c			\
	$(PARSER_DIR)/verilog.c			\
	$(PARSER_DIR)/vhdl.c			\
	$(PARSER_DIR)/vim.c			\
	$(PARSER_DIR)/windres.c			\
	$(PARSER_DIR)/yacc.c

MAIN_SOURCES =					\
	$(MAIN_DIR)/args.c			\
	$(MAIN_DIR)/entry.c			\
	$(MAIN_DIR)/field.c			\
	$(MAIN_DIR)/flags.c			\
	$(MAIN_DIR)/fmt.c			\
	$(MAIN_DIR)/get.c			\
	$(MAIN_DIR)/htable.c			\
	$(MAIN_DIR)/keyword.c			\
	$(MAIN_DIR)/lregex.c			\
	$(MAIN_DIR)/lxcmd.c			\
	$(MAIN_DIR)/main.c			\
	$(MAIN_DIR)/mbcs.c			\
	$(MAIN_DIR)/nestlevel.c			\
	$(MAIN_DIR)/options.c			\
	$(MAIN_DIR)/parse.c			\
	$(MAIN_DIR)/pcoproc.c			\
	$(MAIN_DIR)/read.c			\
	$(MAIN_DIR)/routines.c			\
	$(MAIN_DIR)/selectors.c			\
	$(MAIN_DIR)/sort.c			\
	$(MAIN_DIR)/strlist.c			\
	$(MAIN_DIR)/vstring.c

DEBUG_SOURCES = $(MAIN_DIR)/debug.c

SOURCES = $(MAIN_SOURCES) $(PARSER_SOURCES) $(DEBUG_SOURCES)

ENVIRONMENT_HEADERS = e_msoft.h

ENVIRONMENT_SOURCES = 

REGEX_SOURCES = gnu_regex/regex.c

REGEX_HEADERS = gnu_regex/regex.h

FNMATCH_SOURCES = fnmatch/fnmatch.c

FNMATCH_HEADERS = fnmatch/fnmatch.h

OBJECTS = \
	$(SOURCES:.c=.$(OBJEXT)) \
	$(LIBOBJS)

# vim: ts=8
