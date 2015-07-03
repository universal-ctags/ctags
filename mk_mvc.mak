#
# Makefile for Win32 using Microsoft Visual Studio 2013
#
# To use from the command line:
# 1. From the Start Menu "Visual Studio 2013" -> "Visual Studio Tools" -> "VS2013 x86 Native Tools Command Prompt"
# 2. In the command prompt that opens goto the directory containing the sources
# 3. Execute: nmake -f mk_mvc.mak
#

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -Dbool=int -Dfalse=0 -Dtrue=1 -Dstrcasecmp=stricmp
DEFINES = -DWIN32 $(REGEX_DEFINES)
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch
OPT = /O2
!if "$(WITH_ICONV)" == "yes"
DEFINES = $(DEFINES) -DHAVE_ICONV
LIBS = $(LIBS) -liconv
!endif

ctags: ctags.exe

ctags.exe: respmvc
	cl $(OPT) /Fe$@ @respmvc /link setargv.obj $(LIBS)

readtags.exe: readtags.c
	cl /clr $(OPT) /Fe$@ $(DEFINES) -DREADTAGS_MAIN readtags.c /link setargv.obj

# Debug version
dctags.exe: respmvc
	cl /Zi -DDEBUG /Fe$@ @respmvc debug.c /link setargv.obj

regex.obj:
	cl /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) gnu_regex/regex.c

fnmatch.obj:
	cl /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) fnmatch/fnmatch.c

respmvc: $(SOURCES) $(REGEX_SOURCES) $(FNMATCH_SOURCES) $(HEADERS) $(REGEX_HEADERS) $(FNMATCH_HEADERS) mk_mvc.mak
	echo $(DEFINES) > $@
	echo $(INCLUDES) >> $@
	echo $(SOURCES) >> $@
	echo $(REGEX_SOURCES) >> $@
	echo $(FNMATCH_SOURCES) >> $@

mostlyclean:
	- del *.obj
	- del dctags.exe
	- del respmvc
	- del tags

clean: mostlyclean
	- del ctags.exe
