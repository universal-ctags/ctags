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
INCLUDES = -I. -Imain -Ignu_regex -Ifnmatch -Iparsers
OPT = /O2
REGEX_OBJS = $(REGEX_SRCS:.c=.obj)
FNMATCH_OBJS = $(FNMATCH_SRCS:.c=.obj)
ALL_OBJS = $(ALL_SRCS:.c=.obj) $(REGEX_OBJS) $(FNMATCH_OBJS)
READTAGS_OBJS = $(READTAGS_SRCS:.c=.obj)

!if "$(WITH_ICONV)" == "yes"
DEFINES = $(DEFINES) -DHAVE_ICONV
LIBS = $(LIBS) /libpath:$(ICONV_DIR)/lib iconv.lib
INCLUDES = $(INCLUDES) -I$(ICONV_DIR)/include
!endif

!ifdef DEBUG
DEFINES = $(DEFINES) -DDEBUG
OPT = $(OPT) /Zi
!endif

{main}.c{main}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Fomain\ /c $<
{optlib}.c{optlib}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Fooptlib\ /c $<
{parsers}.c{parsers}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Foparsers\ /c $<
{parsers\cxx}.c{parsers\cxx}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Foparsers\cxx\ /c $<
{read}.c{read}.obj::
	$(CC) $(OPT) $(DEFINES) $(INCLUDES) /Foread\ /c $<

all: ctags.exe readtags.exe

ctags: ctags.exe

ctags.exe: $(ALL_OBJS) $(ALL_HEADS) $(REGEX_HEADS) $(FNMATCH_HEADS)
	$(CC) $(OPT) /Fe$@ $(ALL_OBJS) /link setargv.obj $(LIBS)

readtags.exe: $(READTAGS_OBJS) $(READTAGS_HEADS)
	$(CC) $(OPT) /Fe$@ $(READTAGS_OBJS) /link setargv.obj

$(REGEX_OBJS): $(REGEX_SRCS)
	$(CC) /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) $(REGEX_SRCS)

$(FNMATCH_OBJS): $(FNMATCH_SRCS)
	$(CC) /c $(OPT) /Fo$@ $(INCLUDES) $(DEFINES) $(FNMATCH_SRCS)


clean:
	- del *.obj main\*.obj optlib\*.obj parsers\*.obj parsers\cxx\*.obj gnu_regex\*.obj fnmatch\*.obj read\*.obj
	- del ctags.exe readtags.exe
	- del tags
