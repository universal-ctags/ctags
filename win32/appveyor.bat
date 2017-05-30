@echo off
:: Batch file for building/testing ctags on AppVeyor
::
:: Copyright: 2015 K. Takata
:: License: GPL-2


cd %APPVEYOR_BUILD_FOLDER%
if "%1"=="" (
  set target=build
) else (
  set target=%1
)

for %%i in (msbuild msvc msys2 mingw cygwin) do if "%compiler%"=="%%i" goto %compiler%_%target%

echo Unknown build target.
exit 1

:msbuild_build
:: ----------------------------------------------------------------------
:: Using VC12 (VC2013) with msbuild, iconv disabled
cd win32
@echo on
msbuild ctags_vs2013.sln /logger:"C:\Program Files\AppVeyor\BuildAgent\Appveyor.MSBuildLogger.dll" /p:Configuration=%CONFIGURATION%

@echo off
goto :eof

:msbuild_test
cd win32
@echo on
:: Check filetype
c:\cygwin\bin\file %CONFIGURATION%\ctags.exe
:: Check if it works
%CONFIGURATION%\ctags --version || exit 1

@echo off
goto :eof

:msbuild_package
:: Do nothing.
goto :eof


:msvc_build
:: ----------------------------------------------------------------------
:: Using VC12 (VC2013) with nmake, iconv enabled
:: Also build with msys2 and test the VC binary on msys2.
set MSYS2_ARCH=x86_64
set MSYS2_DIR=msys64
set MSYSTEM=MINGW64
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" %ARCH%

:: Build libiconv (MSVC port)
set ICONV_BUILD_DIR=C:\projects\libiconv
set "INCLUDE=%INCLUDE%;C:\Program Files (x86)\Microsoft SDKs\Windows\v7.1A\Include"
git clone -q --branch=master --depth=1 https://github.com/koron/libiconv.git %ICONV_BUILD_DIR%
cd %ICONV_BUILD_DIR%\msvc10
nmake NODEBUG=1 NOMSVCRT=1

:: Install libiconv to %ICONV_DIR%
set ICONV_DIR=C:\projects\iconv
mkdir %ICONV_DIR%\include
mkdir %ICONV_DIR%\lib
copy %ICONV_BUILD_DIR%\msvc10\iconv.h   %ICONV_DIR%\include     > nul
copy %ICONV_BUILD_DIR%\msvc10\iconv.lib %ICONV_DIR%\lib         > nul
copy %ICONV_BUILD_DIR%\msvc10\iconv.dll %APPVEYOR_BUILD_FOLDER% > nul

:: Build ctags with nmake
@echo on
cd %APPVEYOR_BUILD_FOLDER%
nmake -f mk_mvc.mak WITH_ICONV=yes ICONV_DIR=%ICONV_DIR% PDB=yes

:: Backup VC binaries
mkdir vc
move *.exe vc > nul

:: Build with msys2
path C:\%MSYS2_DIR%\usr\bin;%PATH%
set CHERE_INVOKING=yes
:: Install and update necessary packages
rem bash -lc "for i in {1..3}; do pacman --noconfirm -S mingw-w64-%MSYS2_ARCH%-{python3-sphinx,jansson,libxml2,libyaml} && break || sleep 15; done"

bash -lc "./autogen.sh"
:: Patching configure.
:: Workaround for "./configure: line 557: 0: Bad file descriptor"
perl -i".bak" -pe "s/^test -n \".DJDIR\"/#$&/" configure
bash -lc "./configure && make"

:: Restore VC binaries
copy vc\*.exe . /y > nul
touch *.exe

@echo off
goto :eof

:msvc_test
@echo on
:: Check filetype (VC binaries)
c:\cygwin\bin\file ctags.exe
c:\cygwin\bin\file readtags.exe
:: Check if it works
.\ctags --version || exit 1

:: Run tests
bash -lc "make check APPVEYOR=1"

@echo off
goto :eof

:msvc_package
:: Do nothing.
goto :eof


:msys2_build
:: ----------------------------------------------------------------------
:: Using msys2
@echo on
PATH C:\%MSYS2_DIR%\%MSYSTEM%\bin;C:\%MSYS2_DIR%\usr\bin;%PATH%
set CHERE_INVOKING=yes
:: Install and update necessary packages
bash -lc "for i in {1..3}; do pacman --noconfirm -S mingw-w64-%MSYS2_ARCH%-{python3-sphinx,jansson,libxml2,libyaml} && break || sleep 15; done"

bash -lc "./autogen.sh"
:: Patching configure.
:: Workaround for "./configure: line 557: 0: Bad file descriptor"
perl -i".bak" -pe "s/^test -n \".DJDIR\"/#$&/" configure
:: Use static link.
bash -lc "./configure --enable-iconv --disable-external-sort EXTRA_CFLAGS=-DLIBXML_STATIC LDFLAGS=-static LIBS='-lz -llzma -lws2_32' && make"

@echo off
goto :eof

:msys2_test
@echo on
:: Check filetype (msys2 binaries)
c:\cygwin\bin\file ctags.exe
c:\cygwin\bin\file readtags.exe
:: Check if it works
.\ctags --version || exit 1

:: Run tests
bash -lc "make check APPVEYOR=1"

@echo off
goto :eof

:msys2_package
md package
:: Build html docs
bash -lc "cd docs; make html"
move docs\_build\html package\docs > nul
rd /s/q package\docs\_sources

:: Get version
if "%APPVEYOR_REPO_TAG_NAME%"=="" (
  for /f %%i in ('git rev-parse --short HEAD') do set ver=%%i
) else (
  set ver=%APPVEYOR_REPO_TAG_NAME%
)

:: Create zip package
set filelist=ctags.exe readtags.exe README.md
robocopy . package %filelist% > nul
robocopy win32\license package\license > nul
copy COPYING package\license > nul
copy win32\mkstemp\COPYING.MinGW-w64-runtime.txt package\license > nul
cd package
7z a ..\ctags-%ver%-%ARCH%.debug.zip %filelist% docs license
strip *.exe
7z a ..\ctags-%ver%-%ARCH%.zip %filelist% docs license
cd ..
goto :eof


:mingw_build
:: ----------------------------------------------------------------------
:: Using MinGW without autotools, iconv disabled
@echo on
path C:\MinGW\bin;C:\MinGW\msys\1.0\bin;%path%
make -f mk_mingw.mak

@echo off
goto :eof

:mingw_test
@echo on
:: Check filetype
c:\cygwin\bin\file ctags.exe
c:\cygwin\bin\file readtags.exe
:: Check if it works
.\ctags --version || exit 1

@echo off
goto :eof

:mingw_package
:: Do nothing.
goto :eof


:cygwin_build
:: ----------------------------------------------------------------------
:: Using Cygwin, iconv enabled
@echo on
c:\cygwin\setup-x86.exe -qnNdO -R C:/cygwin -s http://cygwin.mirror.constant.com -l C:/cygwin/var/cache/setup -P dos2unix,libiconv-devel
PATH c:\cygwin\bin;%PATH%
set CHERE_INVOKING=yes
bash -lc ""
bash -lc "./autogen.sh"
:: Patching configure.
:: Workaround for "./configure: line 557: 0: Bad file descriptor"
perl -i".bak" -pe "s/^test -n \".DJDIR\"/#$&/" configure
bash -lc "./configure --enable-iconv && make"

@echo off
goto :eof

:cygwin_test
@echo on
:: Check filetype
c:\cygwin\bin\file ctags.exe
c:\cygwin\bin\file readtags.exe
:: Check if it works
.\ctags --version || exit 1
:: Run tests
bash -lc "make check APPVEYOR=1"

@echo off
goto :eof

:cygwin_package
:: Do nothing.
goto :eof
