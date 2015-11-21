# Makefile for Universal Ctags under Win32 with MinGW compiler

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -Dbool=int -Dfalse=0 -Dtrue=1 -Dstrcasecmp=stricmp

CFLAGS = -Wall
DEFINES = -DWIN32 $(REGEX_DEFINES)
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch
CC = gcc
OBJEXT = o
OBJECTS += $(REGEX_SRCS:%.c=%.o)
OBJECTS += $(FNMATCH_SRCS:%.c=%.o)
VPATH = . ./main ./parsers
ifeq (yes, $(WITH_ICONV))
DEFINES += -DHAVE_ICONV
LIBS += -liconv
endif

ctags.exe: OPT = -O4 -Os -fexpensive-optimizations
ctags.exe: LDFLAGS = -s
dctags.exe: OPT = -g
dctags.exe: DEBUG = -DDEBUG
dctags.exe: ALL_SRCS += debug.c

.SUFFIXES: .c .o

#
# Silent/verbose commands
#
# when V is not set the output of commands is ommited or simplified
#
V	 ?= 0

SILENT   = $(SILENT_$(V))
SILENT_0 = @
SILENT_1 =

V_CC	 = $(V_CC_$(V))
V_CC_0	 = @echo [CC] $@;
V_CC_1	 =


.c.o:
	$(V_CC) $(CC) -c $(OPT) $(CFLAGS) $(DEFINES) $(INCLUDES) -o $@ $<

ctags: ctags.exe
dctags: dctags.exe

ctags.exe dctags.exe: $(OBJECTS) $(HEADERS_ALL) $(HEADERS_REGEX) $(HEADERS_FNMATCH)
	$(V_CC) $(CC) $(OPT) $(CFLAGS) $(LDFLAGS) $(DEFINES) $(INCLUDES) -o $@ $(OBJECTS) $(LIBS)

readtags.exe: readtags.c
	$(V_CC) $(CC) $(OPT) $(CFLAGS) -DREADTAGS_MAIN $(DEFINES) $(INCLUDES) -o $@ $<

clean:
	$(SILENT) echo Cleaning
	$(SILENT) rm -f ctags.exe
	$(SILENT) rm -f dctags.exe
	$(SILENT) rm -f tags
	$(SILENT) rm -f main/*.o parsers/*.o gnu_regex/*.o fnmatch/*.o
