@echo off
:: Copyright (C) 2017 Ken Takata
:: License: GPL-2 or later

setlocal

if "%1"=="" (
  echo Usage: gen-repoinfo ^<headerfile^>
  goto :eof
)
set repoinfo_header=%1

set oldinfo=
if exist %repoinfo_header% (
  for /f "delims=" %%i in (%1) do set oldinfo=%%i
) else (
  type nul > %repoinfo_header%
)

set newinfo=%oldinfo%
if exist .git (
  for /f %%i in ('cmd /c "git describe --tag --exact-match HEAD 2> nul || git rev-parse --short HEAD"') do set newinfo=#define CTAGS_REPOINFO "%%i"
)

if not "%newinfo%"=="%oldinfo%" echo %newinfo%> %repoinfo_header%
