# $Id$
#
# Makefile for Win32 using Microsoft Visual C++ compiler

include source.mak

# You can obtain an Win32 version of the Gnu regex support library from
#   http://people.delphiforums.com/gjc/gnu_regex.html
# Point REGEX_DIR to the directory created when you extract the archive.
# If you just run gnu_regex.exe in this directory, then you can just
# uncomment the REGEX_DIR macro below and everything should work.

#REGEX_DIR = gnu_regex_dist

!ifdef REGEX_DIR
EXTRA_INC = -I$(REGEX_DIR)

# Uncomment the following macro to dynamically link against the regex DLL;
# otherwise link statically against regex.

#REGEX_DLL=1

!ifdef REGEX_DLL
REGEX_DEFINE = -DHAVE_REGCOMP
EXTRA_LIBS = $(REGEX_DIR)\gnu_regex.lib
!else
REGEX_DEFINE = -DHAVE_REGCOMP -DREGEX_MALLOC -DSTDC_HEADERS=1
EXTRA_LIBS = regex.obj
!endif
!endif

DEFINES = -DWIN32 $(REGEX_DEFINE)
INCLUDES = $(EXTRA_INC)
OPT = /O2 /G5

ctags: ctags.exe

ctags.exe: $(SOURCES) respmvc $(EXTRA_LIBS)
	cl $(OPT) /Fe$@ @respmvc /link setargv.obj

readtags.exe: readtags.c
	cl /clr $(OPT) /Fe$@ $(DEFINES) -DREADTAGS_MAIN readtags.c /link setargv.obj

# Debug version
dctags.exe: $(SOURCES) respmvc $(EXTRA_LIBS)
	cl /Zi -DDEBUG /Fe$@ @respmvc debug.c /link setargv.obj

regex.obj:
	cl /c $(OPT) /Fo$@ $(DEFINES) -Dconst= $(INCLUDES) $(REGEX_DIR)\regex.c

respmvc: $(SOURCES) $(HEADERS) mk_mvc.mak
	echo $(DEFINES) $(INCLUDES) $(SOURCES) $(EXTRA_LIBS) > $@

clean:
	- del *.obj
	- del ctags.exe
	- del dctags.exe
	- del respmvc
	- del tags
