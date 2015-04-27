# Shared macros

PARSER_DIR=parsers
MAIN_DIR=main

MAIN_HEADERS = args.h ctags.h debug.h entry.h flags.h general.h get.h htable.h keyword.h \
	main.h options.h parse.h parsers.h pcoproc.h read.h routines.h sort.h \
	strlist.h trashbox.h vstring.h

PARSER_HEADERS =

HEADERS = \
	$(addprefix $(MAIN_DIR)/,$(MAIN_HEADERS))     \
	$(addprefix $(PARSER_DIR)/,$(PARSER_HEADERS))

PARSER_SOURCES =				\
	ada.c					\
	ant.c					\
	asm.c					\
	asp.c					\
	awk.c					\
	basic.c					\
	beta.c					\
	c.c					\
	clojure.c				\
	css.c					\
	cobol.c					\
	dosbatch.c				\
	eiffel.c				\
	erlang.c				\
	falcon.c				\
	flex.c					\
	fortran.c				\
	go.c					\
	html.c					\
	jscript.c				\
	json.c					\
	lisp.c					\
	lua.c					\
	make.c					\
	matlab.c				\
	objc.c					\
	ocaml.c					\
	pascal.c				\
	perl.c					\
	php.c					\
	python.c				\
	rexx.c					\
	ruby.c					\
	rust.c					\
	scheme.c				\
	sh.c					\
	slang.c					\
	sml.c					\
	sql.c					\
	tcl.c					\
	tex.c					\
	verilog.c				\
	vhdl.c					\
	vim.c					\
	windres.c				\
	yacc.c

MAIN_SOURCES =					\
	args.c					\
	entry.c					\
	flags.c					\
	get.c					\
	htable.c				\
	keyword.c				\
	lregex.c				\
	lxcmd.c					\
	main.c					\
	options.c				\
	parse.c					\
	pcoproc.c				\
	read.c					\
	routines.c				\
	sort.c					\
	strlist.c				\
	tg.c					\
	trashbox.c				\
	vstring.c

SOURCES = \
	$(addprefix $(MAIN_DIR)/,$(MAIN_SOURCES))     \
	$(addprefix $(PARSER_DIR)/,$(PARSER_SOURCES))

ENVIRONMENT_HEADERS = e_msoft.h

ENVIRONMENT_SOURCES = 

REGEX_SOURCES = gnu_regex/regex.c

REGEX_HEADERS = gnu_regex/regex.h

FNMATCH_SOURCES = fnmatch/fnmatch.c

FNMATCH_HEADERS = fnmatch/fnmatch.h

OBJECTS = \
	$(SOURCES:.c=.$(OBJEXT)) \
	$(LIBOBJS)
