# $Id$
#
# Makefile for ctags on QDOS/SMS systems and C68 v4.24
# Submitted by Thierry Godefroy <godefroy@imaginet.fr>

# Directories:

T = ram1_
P = drv1_C68_

# Programs name:

CC  = $(P)cc
AS  = $(P)as68
ASM = $(P)qmac
LD  = $(P)ld

# Programs flags:

CCFLAGS  = -tmp$(T) -v -Y$(P) -I$(P)include_ -O
ASFLAGS  = -V
ASMFLAGS = -nolist
LDFLAGS  = -v -L$(P)lib_ -bufp150K\

# Target name:

EXEC = ctags

# Additional libraries:

LIBS =

# Target dependencies:

OBJEXT = o

HEADERS = \
	args.h ctags.h debug.h entry.h general.h get.h keyword.h \
	main.h options.h parse.h parsers.h read.h sort.h strlist.h vstring.h

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

$(EXEC) : $(OBJECTS)
    $(LD) -o$(EXEC) $(LDFLAGS) $(OBJECTS) $(LIBS)

$(OBJECTS): $(HEADERS)

# Construction rules:

_c_o :
    $(CC) -c $(CCFLAGS) $<

_s_o :
    $(AS) $(ASFLAGS) $< $@

_asm_rel :
    $(ASM) $< $(ASMFLAGS)

#end
