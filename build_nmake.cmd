@setlocal enabledelayedexpansion
copy /Y "%~dp0win32\config.h" "%~dp0config.h"
nmake -f "%~dp0win32\Makefile" %*
