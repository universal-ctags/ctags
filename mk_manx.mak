# $Id$
#
# Makefile for ctags on the Amiga, using Aztec/Manx C 5.0 or later

OBJEXT = o

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
	main.$(OBJEXT) \
	make.$(OBJEXT) \
	options.$(OBJEXT) \
	parse.$(OBJEXT) \
	pascal.$(OBJEXT) \
	perl.$(OBJEXT) \
	php.$(OBJEXT) \
	python.$(OBJEXT) \
	read.$(OBJEXT) \
	regex.$(OBJEXT) \
	rexx.$(OBJEXT) \
	ruby.$(OBJEXT) \
	scheme.$(OBJEXT) \
	sh.$(OBJEXT) \
	slang.$(OBJEXT) \
	sort.$(OBJEXT) \
	strlist.$(OBJEXT) \
	tcl.$(OBJEXT) \
	vim.$(OBJEXT) \
	yacc.$(OBJEXT) \
	vstring.$(OBJEXT) \

CC = cc

#>>>>> choose between debugging (-bs) or optimizing (-so)
OPTIONS = -so
#OPTIONS = -bs

#>>>>>> choose -g for debugging
LN_DEBUG =
#LN_DEBUG = -g

CFLAGS = $(OPTIONS) -wapruq -ps -qf -DAMIGA -Dconst=

Ctags: $(OBJECTS)
	ln +q -m $(LN_DEBUG) -o Ctags $(OBJECTS) -lc16 -lm16

.c.o:
	$(CC) $(CFLAGS) -o $*.o $*.c
