# $Id$
#
# Makefile for SAS/C Amiga Compiler
# Submitted by Stefan Haubenthal <polluks@freeshell.org>

CFLAGS= def AMIGA opt parm r sint

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
	lregex.$(OBJEXT) \
	rexx.$(OBJEXT) \
	routines.$(OBJEXT) \
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

ctags: $(OBJECTS)
	sc link to $@ $(OBJECTS) math s sint

.c.o:
	$(CC) $(CFLAGS) -o $*.o $*.c

clean:
	-delete $(OBJECTS) ctags.lnk

archive: clean
	@-delete force RAM:ctags.lha
	lha -r a RAM:ctags // ctags
