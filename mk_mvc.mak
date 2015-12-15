#
# Makefile for Win32 using Microsoft Visual Studio 2013
#
# To use from the command line:
# 1. From the Start Menu "Visual Studio 2013" -> "Visual Studio Tools" -> "VS2013 x86 Native Tools Command Prompt"
# 2. In the command prompt that opens goto the directory containing the sources
# 3. Execute: nmake -f mk_mvc.mak
#

include source.mak

OBJEXT = obj
REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -Dbool=int -Dfalse=0 -Dtrue=1 -Dstrcasecmp=stricmp
DEFINES = -DWIN32 $(REGEX_DEFINES)
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch
OPT = /O2

!if "$(WITH_ICONV)" == "yes"
DEFINES = $(DEFINES) -DHAVE_ICONV
LIBS = $(LIBS) /libpath:$(ICONV_DIR)/lib iconv.lib
INCLUDES = $(INCLUDES) -I$(ICONV_DIR)/include
!endif

!ifdef DEBUG
DEFINES = $(DEFINES) -DDEBUG
OPT = $(OPT) /Zi
!endif

all: ctags.exe readtags.exe

ctags: ctags.exe

ctags.exe: respmvc
	cl $(OPT) /Fe$@ @respmvc /link setargv.obj $(LIBS)

readtags.exe: readtags.c
	cl $(OPT) /Fe$@ $(DEFINES) -DREADTAGS_MAIN readtags.c /link setargv.obj

$(REGEX_OBJS): $(REGEX_SRCS)
	cl /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) $(REGEX_SRCS)

$(FNMATCH_OBJS): $(FNMATCH_SRCS)
	cl /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) $(FNMATCH_SRCS)

respmvc: $(REGEX_OBJS) $(FNMATCH_OBJS) $(ALL_SRCS) $(REGEX_SRCS) $(FNMATCH_SRCS) $(ALL_HEADS) $(REGEX_HEADS) $(FNMATCH_HEADS) mk_mvc.mak
	echo $(DEFINES) > $@
	echo $(INCLUDES) >> $@
	echo $(ALL_SRCS) >> $@
	echo $(REGEX_SRCS) >> $@
	echo $(FNMATCH_SRCS) >> $@

clean:
	- del *.obj
	- del ctags.exe readtags.exe
	- del respmvc
	- del tags
