# $Id$
#
# The most simplistic Makefile, for DJGPP Version 2 on Windows
#
# Rather than using this makefile, it is preferable to run "configure", then
# "make" under BASH on DJGPP (i.e. the standard means of building a package on
# Unix), but you have to have a fuller complement of DJGPP packages installed
# to do this.

include source.mak

CFLAGS	= -O2 -Wall -DMSDOS

ctags.exe: $(SOURCES)
	gcc $(CFLAGS) -s -o ctags.exe $(SOURCES) -lpc

clean:
	del ctags.exe
