# $Id$
#
# Makefile for Exuberant Ctags under Win32 with Mingw32 compiler
#
# Note that only the crtdll variant of the compiler is supported.
# The msvcrt variant has broken implementations of fgetpos() and fsetpos().

include source.mak

CFLAGS = -Wall
DEFINES = -DWIN32
CC = gcc

ctags: ctags.exe

ctags.exe: OPT = -O4
ctags.exe: LDFLAGS = -s
dctags.exe: OPT = -g

ctags.exe dctags.exe: $(SOURCES)
	$(CC) $(LDFLAGS) $(OPT) $(CFLAGS) $(DEFINES) -o $@ $(SOURCES)

clean:
	- del ctags.exe
	- del dctags.exe
	- del tags
