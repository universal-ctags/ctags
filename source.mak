# $Id$
#
# Shared macros

HEADERS = \
	args.h ctags.h debug.h entry.h general.h get.h keyword.h \
	main.h options.h parse.h parsers.h read.h routines.h sort.h \
	strlist.h vstring.h

SOURCES = \
	args.c \
	asm.c \
	asp.c \
	awk.c \
	eiffel.c \
	beta.c \
	c.c \
	cobol.c \
	entry.c \
	fortran.c \
	get.c \
	keyword.c \
	lisp.c \
	lregex.c \
	lua.c \
	main.c \
	make.c \
	options.c \
	parse.c \
	pascal.c \
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
	sml.c \
	sql.c \
	strlist.c \
	tcl.c \
	verilog.c \
	vim.c \
	yacc.c \
	vstring.c

ENVIRONMENT_HEADERS = \
    e_amiga.h e_cygwin.h e_djgpp.h e_mac.h e_msoft.h e_os2.h e_qdos.h \
    e_riscos.h e_vms.h

ENVIRONMENT_SOURCES = \
    argproc.c mac.c qdos.c

OBJECTS = \
	args.$(OBJEXT) \
	asm.$(OBJEXT) \
	asp.$(OBJEXT) \
	awk.$(OBJEXT) \
	eiffel.$(OBJEXT) \
	beta.$(OBJEXT) \
	c.$(OBJEXT) \
	cobol.$(OBJEXT) \
	entry.$(OBJEXT) \
	fortran.$(OBJEXT) \
	get.$(OBJEXT) \
	keyword.$(OBJEXT) \
	lisp.$(OBJEXT) \
	lregex.$(OBJEXT) \
	lua.$(OBJEXT) \
	main.$(OBJEXT) \
	make.$(OBJEXT) \
	options.$(OBJEXT) \
	parse.$(OBJEXT) \
	pascal.$(OBJEXT) \
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
	sml.$(OBJEXT) \
	sql.$(OBJEXT) \
	strlist.$(OBJEXT) \
	tcl.$(OBJEXT) \
	verilog.$(OBJEXT) \
	vim.$(OBJEXT) \
	yacc.$(OBJEXT) \
	vstring.$(OBJEXT)

