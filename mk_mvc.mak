#
# Makefile for Win32 using Microsoft Visual Studio 2013
#
# To use from the command line:
# 1. From the Start Menu "Visual Studio 2013" -> "Visual Studio Tools" -> "VS2013 x86 Native Tools Command Prompt"
# 2. In the command prompt that opens goto the directory containing the sources
# 3. Execute: nmake -f mk_mvc.mak
#

include source.mak

REGEX_DEFINES = -DHAVE_REGCOMP -D__USE_GNU -DHAVE_STDBOOL_H -Dstrcasecmp=stricmp

OBJEXT = obj
COMMON_DEFINES =
DEFINES = -DWIN32 $(REGEX_DEFINES) -DHAVE_PACKCC $(COMMON_DEFINES) -DHAVE_REPOINFO_H
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch -Iparsers -Ilibreadtags -Idsl
OPT = /O2 /WX
PACKCC = packcc.exe
REGEX_OBJS = $(REGEX_SRCS:.c=.obj)
FNMATCH_OBJS = $(FNMATCH_SRCS:.c=.obj)
WIN32_OBJS = $(WIN32_SRCS:.c=.obj)
PEG_OBJS = $(PEG_SRCS:.c=.obj)
PACKCC_OBJS = $(PACKCC_SRCS:.c=.obj)
RES_OBJ = win32/ctags.res
EXTRA_OBJS = $(REGEX_OBJS) $(FNMATCH_OBJS) $(WIN32_OBJS) $(PEG_OBJS) $(RES_OBJ)
ALL_OBJS = $(ALL_SRCS:.c=.obj) $(EXTRA_OBJS)
ALL_LIB_OBJS = $(ALL_LIB_SRCS:.c=.obj) $(EXTRA_OBJS)
READTAGS_OBJS = $(READTAGS_SRCS:.c=.obj)
OPTSCRIPT_OBJS = $(OPTSCRIPT_SRCS:.c=.obj)

!if "$(WITH_ICONV)" == "yes"
DEFINES = $(DEFINES) -DHAVE_ICONV
LIBS = $(LIBS) /libpath:$(ICONV_DIR)/lib iconv.lib
INCLUDES = $(INCLUDES) -I$(ICONV_DIR)/include
!endif

!ifdef DEBUG
DEFINES = $(DEFINES) -DDEBUG
PDB = yes
!endif

!ifdef PDB
OPT = $(OPT) /Zi
PDBFLAG = /debug
!else
PDBFLAG =
!endif

# Generate repoinfo.h.
!if [win32\gen-repoinfo.bat $(REPOINFO_HEADS)]
!endif

.SUFFIXES: .peg

{main}.c{main}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Fomain\ /c $<
{optlib}.c{optlib}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Fooptlib\ /c $<
{parsers}.c{parsers}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Foparsers\ /c $<
{parsers\cxx}.c{parsers\cxx}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Foparsers\cxx\ /c $<
{extra-cmds}.c{extra-cmds}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Foextra-cmds\ /c $<
{libreadtags}.c{libreadtags}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Folibreadtags\ /c $<
{dsl}.c{dsl}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Fodsl\ /c $<
{win32\mkstemp}.c{win32\mkstemp}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Fowin32\mkstemp\ /c $<
{peg}.peg{peg}.c::
	$(PACKCC) $<
{peg}.c{peg}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Fopeg\ /c $<

all: $(PACKCC) ctags.exe readtags.exe optscript.exe

ctags: ctags.exe

ctags.exe: $(ALL_OBJS) $(ALL_HEADS) $(PEG_HEADS) $(PEG_EXTRA_HEADS) $(REGEX_HEADS) $(FNMATCH_HEADS) $(WIN32_HEADS) $(REPOINFO_HEADS)
	$(CC) $(OPT) /Fe$@ $(ALL_OBJS) /link setargv.obj $(LIBS) $(PDBFLAG)

readtags.exe: $(READTAGS_OBJS) $(READTAGS_HEADS) $(REGEX_OBJS) $(REGEX_HEADS)
	$(CC) $(OPT) /Fe$@ $(READTAGS_OBJS) $(REGEX_OBJS) /link setargv.obj $(PDBFLAG)

optscript.exe: $(ALL_LIB_OBJS) $(OPTSCRIPT_OBJS) $(ALL_LIB_HEADS) $(OPTSCRIPT_DSL_HEADS) $(WIN32_HEADS)
	$(CC) $(OPT) /Fe$@ $(ALL_LIB_OBJS) $(OPTSCRIPT_OBJS) /link setargv.obj $(LIBS)

$(REGEX_OBJS): $(REGEX_SRCS)
	$(CC) /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) $(REGEX_SRCS)

$(FNMATCH_OBJS): $(FNMATCH_SRCS)
	$(CC) /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) $(FNMATCH_SRCS)

$(PACKCC_OBJS): $(PACKCC_SRCS)
	$(CC) /c $(OPT) /Fo$@ $(INCLUDES) $(COMMON_DEFINES) $(PACKCC_SRCS)

$(PACKCC): $(PACKCC_OBJS)
	$(CC) $(OPT) /Fe$@ $(PACKCC_OBJS) /link setargv.obj $(PDBFLAG)

main\repoinfo.obj: main\repoinfo.c main\repoinfo.h

peg\varlink.c peg\varlink.h: peg\varlink.peg $(PACKCC)
peg\kotlin.c peg\kotlin.h: peg\kotlin.peg $(PACKCC)

$(RES_OBJ): win32/ctags.rc win32/ctags.exe.manifest win32/resource.h
	$(RC) /nologo /l 0x409 /Fo$@ $*.rc


clean:
	- del *.obj main\*.obj optlib\*.obj parsers\*.obj parsers\cxx\*.obj gnu_regex\*.obj fnmatch\*.obj misc\packcc\*.obj peg\*.obj extra-cmds\*.obj libreadtags\*.obj dsl/*.o win32\mkstemp\*.obj win32\*.res main\repoinfo.h
	- del ctags.exe readtags.exe optscript.exe $(PACKCC)
	- del tags
