# Makefile for Universal Ctags under Win32 with MinGW compiler

OBJEXT = o
include source.mak

GNULIB_OBJS = $(MINGW_GNULIB_SRCS:.c=.$(OBJEXT))

CFLAGS = -Wall -std=gnu99
COMMON_DEFINES =
DEFINES = -DWIN32 $(COMMON_DEFINES) -DHAVE_CONFIG_H -DHAVE_PACKCC
INCLUDES = -I. -Ignulib -Ilibreadtags -iquote parsers -iquote main -iquote dsl
CC = gcc
WINDRES = windres
OPTLIB2C = ./misc/optlib2c
PACKCC   = ./packcc.exe
RES_OBJ = win32/ctags.res.o
EXTRA_OBJS  =
EXTRA_OBJS += $(GNULIB_OBJS)
EXTRA_OBJS += $(WIN32_OBJS)
EXTRA_OBJS += $(PEG_OBJS)
EXTRA_OBJS += $(RES_OBJ)
ALL_OBJS   += $(EXTRA_OBJS)
ALL_LIB_OBJS += $(EXTRA_OBJS)
VPATH = . ./gnulib ./main ./parsers ./optlib ./extra-cmds ./libreadtags ./win32

ifeq (yes, $(WITH_ICONV))
DEFINES += -DHAVE_ICONV
LIBS += -liconv
endif
ifeq (yes, $(WITH_PCRE2))
CFLAGS += -DHAVE_LIBPCRE2=1 $(shell pkg-config --cflags libpcre2-8)
LIBS += $(shell pkg-config --libs libpcre2-8)
PARSER_SRCS += $(PCRE2_SRCS)
PARSER_HEADS += $(PCRE2_HEADS)
endif
ifeq (yes, $(WITH_YAML))
CFLAGS += -DHAVE_LIBYAML=1 $(shell pkg-config --cflags yaml-0.1)
LIBS += $(shell pkg-config --libs yaml-0.1)
PARSER_SRCS += $(YAML_SRCS)
PARSER_HEADS += $(YAML_HEADS)
endif
ifeq (yes, $(WITH_XML))
CFLAGS += -DHAVE_LIBXML=1 $(shell pkg-config --cflags libxml-2.0)
LIBS += $(shell pkg-config --libs libxml-2.0)
PARSER_SRCS += $(XML_SRCS)
PARSER_HEADS += $(XML_HEADS)
endif
ifeq (yes, $(WITH_JSON))
CFLAGS += -DHAVE_JANSSON=1 $(shell pkg-config --cflags jansson)
LIBS += $(shell pkg-config --libs jansson)
endif

ifdef DEBUG
DEFINES += -DDEBUG
OPT = -g
else
OPT = -O4 -Os -fexpensive-optimizations
LDFLAGS = -s
endif

.SUFFIXES: .c .o .ctags .peg

#
# Silent/verbose commands
#
# when V is not set the output of commands is omitted or simplified
#
V	 ?= 0
CC_FOR_PACKCC ?= $(CC)

SILENT   = $(SILENT_$(V))
SILENT_0 = @
SILENT_1 =

V_CC	 = $(V_CC_$(V))
V_CC_0	 = @echo [CC] $@;
V_CC_1	 =

V_OPTLIB2C   = $(V_OPTLIB2C_$(V))
V_OPTLIB2C_0 = @echo [OPTLIB2C] $@;
V_OPTLIB2C_1 =

V_PACKCC   = $(V_PACKCC_$(V))
V_PACKCC_0 = @echo [PACKCC] $@;
V_PACKCC_1 =

V_WINDRES   = $(V_WINDRES_$(V))
V_WINDRES_0 = @echo [WINDRES] $@;
V_WINDRES_1 =


.c.o:
	$(V_CC) $(CC) -c $(OPT) $(CFLAGS) $(DEFINES) $(INCLUDES) -o $@ $<

%.c: %.ctags $(OPTLIB2C)
	$(V_OPTLIB2C) $(OPTLIB2C) $< > $@

peg/%.c peg/%.h: peg/%.peg $(PACKCC)
	$(V_PACKCC) $(PACKCC) $<

all: copy_gnulib_heads $(PACKCC) ctags.exe readtags.exe optscript.exe

ctags: ctags.exe

$(PACKCC_OBJ): $(PACKCC_SRC)
	$(V_CC) $(CC_FOR_PACKCC) -c $(OPT) $(CFLAGS) $(COMMON_DEFINES) -o $@ $<

$(PACKCC): $(PACKCC_OBJ)
	$(V_CC) $(CC_FOR_PACKCC) $(OPT) -o $@ $^

ctags.exe: $(ALL_OBJS) $(ALL_HEADS) $(PEG_HEADS) $(PEG_EXTRA_HEADS) $(MINGW_GNULIB_HEADS) $(WIN32_HEADS)
	$(V_CC) $(CC) $(OPT) $(CFLAGS) $(LDFLAGS) $(DEFINES) $(INCLUDES) -o $@ $(ALL_OBJS) $(LIBS)

$(RES_OBJ): win32/ctags.rc win32/ctags.exe.manifest win32/resource.h
	$(V_WINDRES) $(WINDRES) -o $@ -O coff $<

extra-cmds/%.o: extra-cmds/%.c
	$(V_CC) $(CC) -c $(OPT) $(CFLAGS) -DWIN32 $(INCLUDES) -o $@ $<
libreadtags/%.o: libreadtags/%.c
	$(V_CC) $(CC) -c $(OPT) $(CFLAGS) -DWIN32 -Ilibreadtags -o $@ $<

readtags.exe: $(READTAGS_OBJS) $(READTAGS_HEADS) $(UTIL_OBJS) $(UTIL_HEADS) $(READTAGS_DSL_OBJS) $(READTAGS_DSL_HEADS) $(GNULIB_OBJS) $(MINGW_GNULIB_HEADS)
	$(V_CC) $(CC) $(OPT) -o $@ $(READTAGS_OBJS) $(UTIL_OBJS) $(READTAGS_DSL_OBJS) $(GNULIB_OBJS) $(LIBS)

optscript.exe: $(ALL_LIB_OBJS) $(OPTSCRIPT_OBJS) $(ALL_LIB_HEADS) $(OPTSCRIPT_DSL_HEADS) $(WIN32_HEADS)
	$(V_CC) $(CC) $(OPT) $(CFLAGS) $(LDFLAGS) $(DEFINES) $(INCLUDES) -o $@ $(ALL_LIB_OBJS) $(OPTSCRIPT_OBJS) $(LIBS)

copy_gnulib_heads:
	cp win32/config_mingw.h config.h
	cp win32/gnulib_h/langinfo.h win32/gnulib_h/locale.h win32/gnulib_h/unistd.h win32/gnulib_h/fnmatch.h win32/gnulib_h/string.h win32/gnulib_h/wchar.h gnulib

clean:
	$(SILENT) echo Cleaning
	$(SILENT) rm -f ctags.exe readtags.exe optscript.exe $(PACKCC)
	$(SILENT) rm -f tags
	$(SILENT) rm -f main/*.o optlib/*.o parsers/*.o parsers/cxx/*.o gnulib/*.o misc/packcc/*.o peg/*.o extra-cmds/*.o libreadtags/*.o dsl/*.o win32/*.o win32/mkstemp/*.o
	$(SILENT) rm -f config.h gnulib/langinfo.h gnulib/locale.h gnulib/unistd.h gnulib/fnmatch.h gnulib/string.h gnulib/wchar.h
