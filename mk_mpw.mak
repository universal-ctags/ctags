# $Id$
#
# Makefile for Macintosh using MPW
#
# Created by: Maarten L. Hekkelman <maarten@hekkelman.com>

HEADERS	= ¶
	args.h ctags.h debug.h entry.h general.h get.h keyword.h ¶
	main.h options.h parse.h parsers.h read.h sort.h strlist.h vstring.h ¶
	mac.h

SOURCES = ¶
	args.c ¶
	asm.c ¶
	asp.c ¶
	awk.c ¶
	eiffel.c ¶
	beta.c ¶
	c.c ¶
	cobol.c ¶
	entry.c ¶
	fortran.c ¶
	get.c ¶
	keyword.c ¶
	lisp.c ¶
	lua.c ¶
	main.c ¶
	make.c ¶
	options.c ¶
	parse.c ¶
	pascal.c ¶
	perl.c ¶
	php.c ¶
	python.c ¶
	read.c ¶
	regex.c ¶
	rexx.c ¶ ¶
	ruby.c ¶
	scheme.c ¶
	sh.c ¶
	slang.c ¶
	sort.c ¶
	strlist.c ¶
	tcl.c ¶
	vim.c ¶
	yacc.c ¶
	vstring.c ¶
	mac.c

OBJ = ¶
	args.o ¶
	asm.o ¶
	asp.o ¶
	awk.o ¶
	eiffel.o ¶
	beta.o ¶
	c.o ¶
	cobol.o ¶
	entry.o ¶
	fortran.o ¶
	get.o ¶
	keyword.o ¶
	lisp.o ¶
	lua.o ¶
	main.o ¶
	make.o ¶
	options.o ¶
	parse.o ¶
	pascal.o ¶
	perl.o ¶
	php.o ¶
	python.o ¶
	read.o ¶
	regex.o ¶
	rexx.o ¶
	ruby.o ¶
	scheme.o ¶
	sh.o ¶
	slang.o ¶
	sort.o ¶
	strlist.o ¶
	tcl.o ¶
	vim.o ¶
	yacc.o ¶
	vstring.o ¶
	mac.o

LIBS = ¶
	{PPCLibraries}PPCToolLibs.o ¶
	{SharedLibraries}MathLib ¶
	{SharedLibraries}InterfaceLib ¶
	{SharedLibraries}StdCLib ¶
	{MWPPCLibraries}'MSL StdCRuntime.Lib'

CC			= mwcppc
LD			= mwlinkppc

# Using -nodefaults to avoid having {MWCIncludes} in our include paths
# Needed since we're building a MPW Tool and not an application.
COptions	= -nodefaults -i : -i- -i {CIncludes} -opt full
LOptions	= -xm m -stacksize 128

all Ä CTags

CTags Ä TurnOfEcho {OBJ}
	{LD} {LOptions} -o CTags {OBJ} {LIBS}

{OBJ} Ä {HEADERS}

tags Ä CTags
	:CTags -p. {SOURCES} {HEADERS}

clean Ä
	Delete -y -i {OBJ} {CTags} tags

.o Ä .c
	{CC} {depDir}{default}.c -o {targDir}{default}.o {COptions}

TurnOfEcho Ä
	set echo 0
