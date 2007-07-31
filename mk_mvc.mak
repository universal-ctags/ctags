# $Id$
#
# Makefile for Win32 using Microsoft Visual C++ compiler

include source.mak

REGEX_DEFINE = -DHAVE_REGCOMP -DREGEX_MALLOC -DSTDC_HEADERS=1
EXTRA_LIBS = regex.obj

DEFINES = -DWIN32 $(REGEX_DEFINE)
INCLUDES = -I.
OPT = /O2

ctags: ctags.exe

ctags.exe: $(SOURCES) respmvc $(EXTRA_LIBS)
	cl $(OPT) /Fe$@ @respmvc /link setargv.obj

readtags.exe: readtags.c
	cl /clr $(OPT) /Fe$@ $(DEFINES) -DREADTAGS_MAIN readtags.c /link setargv.obj

# Debug version
dctags.exe: $(SOURCES) respmvc $(EXTRA_LIBS)
	cl /Zi -DDEBUG /Fe$@ @respmvc debug.c /link setargv.obj

regex.obj:
	cl /c $(OPT) /Fo$@ $(DEFINES) -Dconst= regex.c

respmvc: $(SOURCES) $(HEADERS) mk_mvc.mak
	echo $(DEFINES) $(INCLUDES) $(SOURCES) $(EXTRA_LIBS) > $@

mostlyclean:
	- del *.obj
	- del dctags.exe
	- del respmvc
	- del tags

clean: mostlyclean
	- del ctags.exe
