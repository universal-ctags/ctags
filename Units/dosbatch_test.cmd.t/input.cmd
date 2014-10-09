@SETLOCAL
@echo off

SET OPTIONS=%1
SET SAMPLE_DATA=%1
if NOT '%SAMPLE_DATA%' EQU '' SET SAMPLE_DATA=1
if '%SAMPLE_DATA%' EQU '' SET SAMPLE_DATA=0
@echo.
@echo.
@echo.Sample data:[%SAMPLE_DATA%]
@echo.
@echo.
 
@IF NOT DEFINED SCRIPT_DIR FOR /f %%i IN ('cd') DO @SET SCRIPT_DIR=%%i


CALL setenv.cmd 
@if NOT "%errorlevel%" EQU "0" goto ERROR
IF DEFINED required_software_missing goto ERROR

set PATH=%SQLANY11%\bin32;%PATH%

@if "%OPTIONS%." EQU "1." set OPTIONS=
@rem Jump to label if specified
@if NOT "%OPTIONS%." EQU "." goto %OPTIONS%

CALL cleanup11.cmd 

:UPDATE
@echo.
@echo.
@echo Update objects (triggers, procedures, web services)
@echo.
:SAMPLE_DATA
@echo.
@echo.
@echo Create report
@echo.
cd %SQL_DIR%
@echo on
%DBISQL% -c "dsn=%CONS%;uid=dba;pwd=%CONS_PWD%;autostop=no" read schema_logging.sql; 
@echo off
@if NOT "%errorlevel%" EQU "0" goto ERROR
@echo errorlevel [%errorlevel%]

:DONE
@echo.
@echo.
@echo.-------------------------------------------
@echo  All Done!
@echo.-------------------------------------------
@echo.
@echo.
goto END

:ERROR
FOR /f %%i IN ('cd') DO @SET CURR_DIR=%%i
@echo.
@echo.
@echo.-------------------------------------------
@echo.    ERROR!!
@echo errorlevel = %errorlevel%
@echo gendb11.cmd 
@echo.
@echo.Working directory:
@echo CURR_DIR=%CURR_DIR%
@echo.-------------------------------------------
@echo.
goto END

:ERROR_OPTIONS
@echo.
@echo.
@echo.
@echo ********************************************************
@echo * Please provide dev, qa, demo as a commandline option *
@echo ********************************************************
@echo.
@echo.
@echo.
GOTO END


:END
cd %SCRIPT_DIR%
@ENDLOCAL
