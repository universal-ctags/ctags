# $Id$
#
# Makefile for Win32 using Borland C++ compiler, version 5.5 (free version)

!include source.mak

BCC = bcc32

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
# WARNING: Borland C++ does not successfully link dynamically.

#REGEX_DLL=1

!ifdef REGEX_DLL
REGEX_DEFINE = -DHAVE_REGCOMP
EXTRA_LIBS = $(REGEX_DIR)\gnu_regex.lib
LDFLAGS = -tWCR
!else
REGEX_DEFINE = -DHAVE_REGCOMP -DREGEX_MALLOC -DSTDC_HEADERS=1
EXTRA_LIBS = regex.obj
!endif
!endif

DEFINES = -DWIN32 $(REGEX_DEFINE)
INCLUDES = $(EXTRA_INC)
WARNINGS = -w-aus -w-par -w-pia -w-pro -w-sus
CFLAGS = -d -DSTRICT -lTpe -lap

# Optimizations if your platform supports all of them.
OPT = -O2 -OS -lGt

# Allows multithreading
#MT_OPT = -tWM -lcw32mt

ctags: ctags.exe

ctags.exe: $(SOURCES) respbc5 $(EXTRA_LIBS)
	$(BCC) $(OPT) $(MT_OPT) -e$@ $(LDFLAGS) @respbc5

readtags.exe: readtags.c
	$(BCC) $(CFLAGS) $(OPT) $(MT_OPT) -e$@ $(DEFINES) -DREADTAGS_MAIN readtags.c $(LDFLAGS)

# Debug version
dctags.exe: $(SOURCES) respbc5 $(EXTRA_LIBS)
	$(BCC) -DDEBUG -e$@ $(LDFLAGS) @respbc5 debug.c

regex.obj:
	$(BCC) -c -o$@ -w- $(DEFINES) -Dconst= $(INCLUDES) $(REGEX_DIR)\regex.c

respbc5: $(SOURCES) $(HEADERS) mk_bc5.mak
	echo $(DEFINES) $(INCLUDES) > $@
	echo $(WARNINGS) >> $@
	echo $(CFLAGS) >> $@
	echo $(SOURCES) $(EXTRA_LIBS) >> $@

clean:
	- del *.obj
	- del *.tds
	- del ctags.exe
	- del dctags.exe
	- del respbc5
	- del tags
