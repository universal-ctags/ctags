@echo off
:: Batch file for building/testing ctags on AppVeyor
::
:: Copyright (C) 2015 Ken Takata
:: License: GPL-2 or later


cd %APPVEYOR_BUILD_FOLDER%
if "%1"=="" (
  set target=build
) else (
  set target=%1
)

:: Daily builds or tag builds should be only built by msys2
set normalbuild=yes
if "%APPVEYOR_SCHEDULED_BUILD%"=="True" (
  set normalbuild=no
)
if not "%APPVEYOR_REPO_TAG_NAME%"=="" (
  set normalbuild=no
)
if "%normalbuild%"=="no" (
  if not "%compiler%"=="msys2" (
    exit 0
  )
)

for %%i in (msbuild msvc msys2 mingw cygwin) do if "%compiler%"=="%%i" goto %compiler%_%target%

echo Unknown build target.
exit 1

:msbuild_build
:: ----------------------------------------------------------------------
:: Using VC12 (VC2013) with msbuild, iconv disabled
@echo on
copy win32\config_mvc.h config.h
copy win32\gnulib_h\langinfo.h gnulib
copy win32\gnulib_h\fnmatch.h gnulib
cd win32
msbuild ctags_vs2013.sln /logger:"C:\Program Files\AppVeyor\BuildAgent\Appveyor.MSBuildLogger.dll" /p:Configuration=%CONFIGURATION%

@echo off
goto :eof

:msbuild_test
cd win32
@echo on
:: Check filetype
c:\cygwin64\bin\file %CONFIGURATION%\ctags.exe
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
:: Also create Makefile with msys2 and test the VC binary on msys2.
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
nmake -f mk_mvc.mak WITH_ICONV=yes ICONV_DIR=%ICONV_DIR% PDB=yes || exit 1

@echo off
goto :eof

:msvc_test
@echo on
:: Prepare for msys2
path C:\%MSYS2_DIR%\usr\bin;%PATH%
set CHERE_INVOKING=yes

:: Check filetype (VC binaries)
c:\cygwin64\bin\file ctags.exe
c:\cygwin64\bin\file readtags.exe
:: Check if it works
.\ctags --version || exit 1

:: Run tests using misc/units.py on msys2
bash -lc "py misc/units.py tmain ./Tmain --ctags=./ctags.exe --readtags=./readtags.exe --show-diff-output --shell=c:/msys64/usr/bin/sh" || exit 1
bash -lc "py misc/units.py run ./Units --ctags=./ctags.exe --show-diff-output --with-timeout=10 --shell=c:/msys64/usr/bin/sh" || exit 1

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
@rem Reduce time required to install packages by disabling pacman's disk space checking.
bash -lc "sed -i 's/^CheckSpace/#CheckSpace/g' /etc/pacman.conf"
if "%normalbuild%"=="no" (
  @rem Change build message: "Daily build: YYYY-MM-DD"
  for /f "tokens=2-4 delims=/ " %%i in ('date /t') do appveyor UpdateBuild -Message "Daily build: %%k-%%i-%%j"

  @rem Remove unused toolchain to reduce the time for updating
  if "%MSYSTEM%"=="MINGW64" (
    bash -lc "pacman --noconfirm -Rs mingw-w64-i686-toolchain"
  ) else if "%MSYSTEM%"=="MINGW32" (
    bash -lc "pacman --noconfirm -Rs mingw-w64-x86_64-toolchain"
  )
  @rem Synchronize package databases and upgrade the core system
  C:\%MSYS2_DIR%\usr\bin\pacman --noconfirm --noprogressbar -Syu
  @rem Run again to update the rest of packages
  C:\%MSYS2_DIR%\usr\bin\pacman --noconfirm --noprogressbar -Su
  @rem Also install packages needed for creating zip package
  bash -lc "for i in {1..3}; do pacman --noconfirm --noprogressbar -S --needed mingw-w64-%MSYS2_ARCH%-python3-sphinx && break || sleep 15; done"
)
:: Install necessary packages
bash -lc "for i in {1..3}; do pacman --noconfirm --noprogressbar -S --needed mingw-w64-%MSYS2_ARCH%-{jansson,libxml2,libyaml,pcre2} autoconf automake && break || sleep 15; done"

bash -lc "./autogen.sh" || exit 1
:: Use static link.
bash -lc "./configure --disable-external-sort --enable-static && make -j2"

@echo off
goto :eof

:msys2_test
@echo on
:: Check filetype (msys2 binaries)
c:\cygwin64\bin\file ctags.exe
c:\cygwin64\bin\file readtags.exe
:: Check if it works
.\ctags --version || exit 1

:: Run tests
if "%normalbuild%-%ARCH%"=="yes-x64" (
  @echo Tests for msys2 x64 are skipped.
  exit 0
)
bash -lc "make check PYTHON=py"

@echo off
goto :eof

:msys2_package
:: Only daily builds or tag builds need to create zip packages
if "%normalbuild%"=="yes" (
  exit 0
)
md package
:: Build html docs and man pages
bash -lc "make -C docs html && make -C man html" || exit 1
move docs\_build\html package\docs > nul
rd /s/q package\docs\_sources

:: Get version
if "%APPVEYOR_REPO_TAG_NAME%"=="" (
  for /f %%i in ('git describe --tags --always') do set ver=%%i
) else (
  set ver=%APPVEYOR_REPO_TAG_NAME%
)
set ver=%ver:/=_%

:: Create zip package
set filelist=ctags.exe readtags.exe README.md
set dirlist=docs license man
robocopy . package %filelist% > nul
robocopy win32\license package\license > nul
copy COPYING package\license > nul
copy win32\mkstemp\COPYING.MinGW-w64-runtime.txt package\license > nul
robocopy man package\man *.html > nul
cd package
for %%i in (*.exe) do call :strip %%i
7z a ..\ctags-%ver%-%ARCH%.zip %filelist% %dirlist%
7z a ..\ctags-%ver%-%ARCH%.debuginfo.zip *.exe.debug
cd ..
goto :eof

:strip
objcopy --only-keep-debug %1 %1.debug
strip %1
objcopy --add-gnu-debuglink=%1.debug %1
goto :eof


:mingw_build
:: ----------------------------------------------------------------------
:: Using MinGW without autotools, iconv disabled
@echo on
path C:\MinGW\bin;C:\MinGW\msys\1.0\bin;%path%
make -f mk_mingw.mak -j2

@echo off
goto :eof

:mingw_test
@echo on
:: Check filetype
c:\cygwin64\bin\file ctags.exe
c:\cygwin64\bin\file readtags.exe
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
c:\cygwin64\setup-x86_64.exe -qnNdO -P dos2unix,libiconv-devel,libjansson-devel,libxml2-devel,libyaml-devel,pcre2,perl
PATH c:\cygwin64\bin;%PATH%
set CHERE_INVOKING=yes
bash -lc "./autogen.sh"
bash -lc "./configure && make -j2"

@echo off
goto :eof

:cygwin_test
@echo on
:: Check filetype
c:\cygwin64\bin\file ctags.exe
c:\cygwin64\bin\file readtags.exe
:: Check if it works
.\ctags --version || exit 1
:: Run tests
bash -lc "make check PYTHON=py"

@echo off
goto :eof

:cygwin_package
:: Do nothing.
goto :eof
