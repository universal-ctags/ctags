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
	ada.$(OBJEXT) \
	args.$(OBJEXT) \
	ant.$(OBJEXT) \
	asm.$(OBJEXT) \
	asp.$(OBJEXT) \
	awk.$(OBJEXT) \
	basic.$(OBJEXT) \
	beta.$(OBJEXT) \
	c.$(OBJEXT) \
	css.$(OBJEXT) \
	cobol.$(OBJEXT) \
	dosbatch.$(OBJEXT) \
	eiffel.$(OBJEXT) \
	entry.$(OBJEXT) \
	erlang.$(OBJEXT) \
	falcon.$(OBJEXT) \
	flags.$(OBJEXT) \
	flex.$(OBJEXT) \
	fortran.$(OBJEXT) \
	get.$(OBJEXT) \
	go.$(OBJEXT) \
	html.$(OBJEXT) \
	htable.$(OBJEXT) \
	jscript.$(OBJEXT) \
	keyword.$(OBJEXT) \
	lisp.$(OBJEXT) \
	lregex.$(OBJEXT) \
	lua.$(OBJEXT) \
	lxcmd.$(OBJEXT) \
	main.$(OBJEXT) \
	make.$(OBJEXT) \
	matlab.$(OBJEXT) \
	objc.$(OBJEXT) \
	ocaml.$(OBJEXT) \
	options.$(OBJEXT) \
	parse.$(OBJEXT) \
	pascal.$(OBJEXT) \
	pcoproc.$(OBJEXT) \
	perl.$(OBJEXT) \
	php.$(OBJEXT) \
	python.$(OBJEXT) \
	read.$(OBJEXT) \
	rexx.$(OBJEXT) \
	routines.$(OBJEXT) \
	ruby.$(OBJEXT) \
	scheme.$(OBJEXT) \
	sh.$(OBJEXT) \
	slang.$(OBJEXT) \
	sml.$(OBJEXT) \
	sort.$(OBJEXT) \
	sql.$(OBJEXT) \
	strlist.$(OBJEXT) \
	tcl.$(OBJEXT) \
	tex.$(OBJEXT) \
	tg.$(OBJEXT) \
	trashbox.$(OBJEXT) \
	verilog.$(OBJEXT) \
	vhdl.$(OBJEXT) \
	vim.$(OBJEXT) \
	windres.$(OBJEXT) \
	yacc.$(OBJEXT) \
	vstring.$(OBJEXT) \
	$(LIBOBJS)
