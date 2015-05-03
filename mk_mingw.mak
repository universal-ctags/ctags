# Makefile for Exuberant Ctags under Win32 with MinGW compiler

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -Dbool=int -Dfalse=0 -Dtrue=1 -Dstrcasecmp=stricmp

CFLAGS = -Wall
DEFINES = -DWIN32 $(REGEX_DEFINES)
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch
CC = gcc
OBJEXT = o
OBJECTS += $(REGEX_SOURCES:%.c=%.o)
OBJECTS += $(FNMATCH_SOURCES:%.c=%.o)
VPATH = . ./main ./parsers

ctags.exe: OPT = -O4 -Os -fexpensive-optimizations
ctags.exe: LDFLAGS = -s
dctags.exe: OPT = -g
dctags.exe: DEBUG = -DDEBUG
dctags.exe: SOURCES += debug.c

.SUFFIXES: .c.o

.c.o:
	$(CC) -c $(OPT) $(CFLAGS) $(DEFINES) $(INCLUDES) -o $@ $<

ctags: ctags.exe
dctags: dctags.exe

ctags.exe dctags.exe: $(OBJECTS) $(HEADERS) $(REGEX_HEADERS) $(FNMATCH_HEADERS)
	$(CC) $(OPT) $(CFLAGS) $(LDFLAGS) $(DEFINES) $(INCLUDES) -o $@ $(OBJECTS)

readtags.exe: readtags.c
	$(CC) $(OPT) $(CFLAGS) -DREADTAGS_MAIN $(DEFINES) $(INCLUDES) -o $@ $<

clean:
	- rm -f ctags.exe
	- rm -f dctags.exe
	- rm -f tags
	- rm -f main/*.o parsers/*.o gnu_regex/*.o fnmatch/*.o
