@echo off
:: Batch file for building/testing ctags on AppVeyor
::
:: Copyright: 2015 K. Takata
:: License: GPL-2


cd %APPVEYOR_BUILD_FOLDER%
if /I "%1"=="test" (
  set _target=_test
) else (
  set _target=
)

for %%i in (msbuild msvc mingw msys2 cygwin) do if "%compiler%"=="%%i" goto %compiler%%_target%

echo Unknown build target.
exit 1

:msbuild
:: ----------------------------------------------------------------------
:: Using VC12 with msbuild, iconv disabled
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


:msvc
:: ----------------------------------------------------------------------
:: Using VC12 with nmake, iconv enabled
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\vcvarsall.bat" %ARCH%

:: Build libiconv (MSVC port)
set ICONV_BUILD_DIR=C:\projects\libiconv
set "INCLUDE=%INCLUDE%;C:\Program Files (x86)\Microsoft SDKs\Windows\v7.1A\Include"
git clone -q --branch=master https://github.com/koron/libiconv.git %ICONV_BUILD_DIR%
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
nmake -f mk_mvc.mak WITH_ICONV=yes ICONV_DIR=%ICONV_DIR%

@echo off
goto :eof

:msvc_test
@echo on
:: Check filetype
c:\cygwin\bin\file ctags.exe
:: Check if it works
.\ctags --version || exit 1

@echo off
goto :eof


:mingw
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
:: Check if it works
.\ctags --version || exit 1

@echo off
goto :eof


:msys2
:: ----------------------------------------------------------------------
:: Using MSYS2, iconv enabled
@echo on
PATH C:\%MSYS2_DIR%\%MSYSTEM%\bin;C:\%MSYS2_DIR%\usr\bin;%PATH%
set CHERE_INVOKING=yes
:: Install and update necessary packages
rem bash -lc "for i in {1..3}; do update-core && break || sleep 15; done"
rem bash -lc "for i in {1..3}; do pacman --noconfirm -Su mingw-w64-%MSYS2_ARCH%-{gcc,libiconv} automake autoconf make dos2unix && break || sleep 15; done"

bash -lc "autoreconf -vfi"
:: Patching configure.
:: Workaround for "./configure: line 557: 0: Bad file descriptor"
perl -i".bak" -pe "s/^test -n \".DJDIR\"/#$&/" configure
bash -lc "./configure --enable-iconv && make"

@echo off
goto :eof

:msys2_test
@echo on
:: Check filetype
c:\cygwin\bin\file ctags.exe
:: Check if it works
.\ctags --version || exit 1
:: Run tests
bash -lc "make check APPVEYOR=1"

@echo off
goto :eof


:cygwin
:: ----------------------------------------------------------------------
:: Using Cygwin, iconv enabled
@echo on
c:\cygwin\setup-x86.exe -qnNdO -R C:/cygwin -s http://cygwin.mirror.constant.com -l C:/cygwin/var/cache/setup -P dos2unix
PATH c:\cygwin\bin;%PATH%
set CHERE_INVOKING=yes
bash -lc ""
bash -lc "autoreconf -vfi"
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
:: Check if it works
.\ctags --version || exit 1
:: Run tests
bash -lc "make check APPVEYOR=1"

@echo off
goto :eof
