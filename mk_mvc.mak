# Makefile for Win32 using Microsoft Visual C++ compiler
# To use from the command line:
# 1. Make sure C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\bin\ is in the PATH
# 2. Make sure C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\
# 3. Execute: C:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\vcvarsall.bat
# 4. Execute: nmake -f mk_mvc.mak

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -Dbool=int -Dfalse=0 -Dtrue=1 -Dstrcasecmp=stricmp
DEFINES = -DWIN32 $(REGEX_DEFINES)
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch
OPT = /O2

ctags: ctags.exe

ctags.exe: respmvc
	cl $(OPT) /Fe$@ @respmvc /link setargv.obj

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
