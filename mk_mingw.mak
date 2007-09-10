# $Id$
#
# Makefile for Exuberant Ctags under Win32 with MinGW compiler
#

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -DREGEX_MALLOC -DSTDC_HEADERS=1

CFLAGS = -Wall
DEFINES = -DWIN32 $(REGEX_DEFINES) $(DEBUG)
INCLUDES = -I. -Ignu_regex
CC = gcc

ctags.exe: OPT = -O4
dctags.exe: OPT = -g
dctags.exe: DEBUG = -DDEBUG
dctags.exe: SOURCES += debug.c

ctags: ctags.exe

ctags.exe dctags.exe: $(SOURCES) $(REGEX_SOURCES) $(HEADERS) $(REGEX_HEADERS)
	$(CC) $(OPT) $(CFLAGS) $(DEFINES) $(INCLUDES) -o $@ $(SOURCES) $(REGEX_SOURCES)

readtags.exe: readtags.c
	$(CC) $(OPT) $(CFLAGS) -DREADTAGS_MAIN $(DEFINES) $(INCLUDES) -o $@ $<

clean:
	- rm -f ctags.exe
	- rm -f dctags.exe
	- rm -f tags
