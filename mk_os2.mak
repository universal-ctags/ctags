# $Id$
#
# A Makefile for OS/2 using EMX/gcc
# You may want to use the OS/2 port of autoconf for building
# and outcomment the according statements in this Makefile
# You need a library to provide regex support.
# libExt might do this, but currently (2/2001) it doesn't work together
# with ctags ...
#
# Provided and supported by Alexander Mai
#   <st002279@hrzpub.tu-darmstadt.de>

default:
	@echo "Enter $(MAKE) -f mk_os2.mak target"
	@echo "where target is one of:"
	@echo "   small    (small executable req. EMX runtime)"
	@echo "   debug    (executable for debugging purposes)"
	@echo "   release  (stand-alone executable)"
	@echo "   clean    (remove all files built)"

# Use this to create a small binary
# (requires EMX runtime libraries)
small:
	$(MAKE) -f mk_os2.mak ctags \
	CC="gcc" \
	CFLAGS="-O4 -mpentium -Wall" \
	LFLAGS="-Zcrtdll -s" \
	LIBS="-lregex"

# Use this to create a binary for debugging purposes
# (requires EMX runtime libraries)
debug:
	$(MAKE) -f mk_os2.mak ctags \
	CC="gcc" \
	CFLAGS="-O0 -Wall -g" \
	LFLAGS="-Zcrtdll -g" \
	LIBS="-lregex"

# Use this to create a stand-alone binary for distribution
# (requires link386 for linking but no EMX runtime libraries)
release:
	$(MAKE) -f mk_os2.mak ctags \
	CC="gcc" \
	CFLAGS="-g -O4 -mpentium -Wall -Zomf" \
	LFLAGS="-s -Zomf -Zsys -Zlinker /PM:VIO" \
	LIBS="-lregex"

# Use the line below if you have created config.h
# (manually or by running configure)
# Otherwise use built-in defaults (#ifdef OS2)!
# DEFINES=-DHAVE_CONFIG_H
DEFINES=-DOS2


# General rules and definitions

.SUFFIXES: .c .exe .h .o .obj

OBJEXT = o

include source.mak

ctags: ctags.exe
etags: etags.exe

ctags.exe: $(OBJECTS)
	$(CC) $(CFLAGS) $(LFLAGS) -o $@ $^ $(LIBS)

etags.exe: ctags.exe
	@copy $< $@

.c.o:
	$(CC) $(CFLAGS) $(DEFINES) -I. -c $< -o $@

.c.obj:
	$(CC) $(CFLAGS) $(DEFINES) -I. -c $< -o $@

# Delete all files that are not part of the source distribution
clean:
	@if exist ctags.exe del ctags.exe
	@if exist etags.exe del etags.exe
	@if exist *.obj     del *.obj
	@if exist *.o       del *.o
