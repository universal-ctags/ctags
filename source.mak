# $Id$
#
# Shared macros

HEADERS = \
	args.h ctags.h debug.h entry.h flags.h general.h get.h htable.h keyword.h \
	main.h options.h parse.h parsers.h pcoproc.h read.h routines.h sort.h \
	strlist.h trashbox.h vstring.h

SOURCES = \
	ada.c \
	args.c \
	ant.c \
	asm.c \
	asp.c \
	awk.c \
	basic.c \
	beta.c \
	c.c \
	css.c \
	cobol.c \
	dosbatch.c \
	eiffel.c \
	entry.c \
	erlang.c \
	falcon.c \
	flags.c \
	flex.c \
	fortran.c \
	get.c \
	go.c \
	html.c \
	htable.c \
	jscript.c \
	json.c \
	keyword.c \
	lisp.c \
	lregex.c \
	lua.c \
	lxcmd.c \
	main.c \
	make.c \
	matlab.c \
	objc.c \
	ocaml.c \
	options.c \
	parse.c \
	pascal.c \
	pcoproc.c \
	perl.c \
	php.c \
	python.c \
	read.c \
	rexx.c \
	routines.c \
	ruby.c \
	scheme.c \
	sh.c \
	slang.c \
	sml.c \
	sort.c \
	sql.c \
	strlist.c \
	tcl.c \
	tex.c \
	tg.c \
	trashbox.c \
	verilog.c \
	vhdl.c \
	vim.c \
	windres.c \
	yacc.c \
	vstring.c

ENVIRONMENT_HEADERS = \
    e_amiga.h e_djgpp.h e_mac.h e_msoft.h e_os2.h e_qdos.h e_riscos.h e_vms.h

ENVIRONMENT_SOURCES = \
    argproc.c mac.c qdos.c

REGEX_SOURCES = gnu_regex/regex.c

REGEX_HEADERS = gnu_regex/regex.h

FNMATCH_SOURCES = fnmatch/fnmatch.c

FNMATCH_HEADERS = fnmatch/fnmatch.h

OBJECTS = \
	$(SOURCES:.c=.$(OBJEXT)) \
	$(LIBOBJS)
