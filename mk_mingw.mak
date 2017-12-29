# Makefile for Universal Ctags under Win32 with MinGW compiler

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -Dbool=int -Dfalse=0 -Dtrue=1 -Dstrcasecmp=stricmp

CFLAGS = -Wall -std=gnu99
DEFINES = -DWIN32 $(REGEX_DEFINES)
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch -Iparsers
CC = gcc
OPTLIB2C = ./misc/optlib2c
OBJEXT = o
ALL_OBJS += $(REGEX_OBJS)
ALL_OBJS += $(FNMATCH_OBJS)
ALL_OBJS += $(WIN32_OBJS)
VPATH = . ./main ./parsers ./optlib ./read ./win32

ifeq (yes, $(WITH_ICONV))
DEFINES += -DHAVE_ICONV
LIBS += -liconv
endif

ifdef DEBUG
DEFINES += -DDEBUG
OPT = -g
else
OPT = -O4 -Os -fexpensive-optimizations
LDFLAGS = -s
endif

.SUFFIXES: .c .o .ctags

#
# Silent/verbose commands
#
# when V is not set the output of commands is omitted or simplified
#
V	 ?= 0

SILENT   = $(SILENT_$(V))
SILENT_0 = @
SILENT_1 =

V_CC	 = $(V_CC_$(V))
V_CC_0	 = @echo [CC] $@;
V_CC_1	 =

V_OPTLIB2C   = $(V_OPTLIB2C_$(V))
V_OPTLIB2C_0 = @echo [OPTLIB2C] $@;
V_OPTLIB2C_1 =


.c.o:
	$(V_CC) $(CC) -c $(OPT) $(CFLAGS) $(DEFINES) $(INCLUDES) -o $@ $<

.ctags.c: $(OPTLIB2C)
	$(V_OPTLIB2C) $(OPTLIB2C) $< > $@

all: ctags.exe readtags.exe

ctags: ctags.exe

ctags.exe: $(ALL_OBJS) $(ALL_HEADS) $(REGEX_HEADS) $(FNMATCH_HEADS) $(WIN32_HEADS)
	$(V_CC) $(CC) $(OPT) $(CFLAGS) $(LDFLAGS) $(DEFINES) $(INCLUDES) -o $@ $(ALL_OBJS) $(LIBS)

read/%.o: read/%.c
	$(V_CC) $(CC) -c $(OPT) $(CFLAGS) -DWIN32 -Iread -o $@ $<

readtags.exe: $(READTAGS_OBJS) $(READTAGS_HEADS)
	$(V_CC) $(CC) $(OPT) -o $@ $(READTAGS_OBJS) $(LIBS)

clean:
	$(SILENT) echo Cleaning
	$(SILENT) rm -f ctags.exe readtags.exe
	$(SILENT) rm -f tags
	$(SILENT) rm -f main/*.o optlib/*.o parsers/*.o parsers/cxx/*.o gnu_regex/*.o fnmatch/*.o read/*.o win32/mkstemp/*.o
