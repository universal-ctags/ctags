# $Id$
#
# Makefile for building CTAGS under OpenVMS
#
# Maintained by by Zoltan Arpadffy <arpadffy@altavista.net>
#
# Edit the lines in the Configuration section below to select.

######################################################################
# Configuration section.
######################################################################
# Compiler selection.
# Comment out if you use the VAXC compiler
######################################################################
DECC = YES

######################################################################
# Uncomment if want a debug version. Resulting executable is DCTAGS.EXE
######################################################################
# DEBUG = YES

######################################################################
# End of configuration section.
#
# Please, do not change anything below without programming experience.
######################################################################

CC      = cc

.IFDEF DECC
CC_DEF  = $(CC)/decc/prefix=all
.ELSE
CC_DEF  = $(CC)
.ENDIF

LD_DEF  = link

.IFDEF DEBUG
TARGET  = dctags.exe
CFLAGS  = /debug/noopt/list/cross_reference/include=[]
LDFLAGS = /debug
.ELSE
TARGET  = ctags.exe
CFLAGS  = /include=[]
LDFLAGS =
.ENDIF

OBJEXT = obj

.SUFFIXES : .obj .c

.INCLUDE source.mak

EXTRA_OBJS = argproc.obj

all : $(TARGET)
        ! $@

.c.obj :  
        $(CC_DEF) $(CFLAGS) $<

$(TARGET) :  $(OBJECTS) $(EXTRA_OBJS)
        $(LD_DEF) $(LDFLAGS) /exe=$(TARGET) $+

clean :
       -@ if F$SEARCH("*.obj") .NES. "" then delete/noconfirm/nolog *.obj.*
       -@ if F$SEARCH("*.exe") .NES. "" then delete/noconfirm/nolog *.exe.*
       -@ if F$SEARCH("config.h") .NES. "" then delete/noconfirm/nolog config.h.*
