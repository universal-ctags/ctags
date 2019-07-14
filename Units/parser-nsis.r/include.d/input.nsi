# Taken from https://nsis.sourceforge.io/Docs/Chapter5.html#include
!include WinMessages.nsh
!include Library.nsh
!include /CHARSET=CP1252 C:\MyConfig.nsi
!include ..\MyConfig.nsh
!include /NONFATAL file_that_may_exist_or_not.nsh


!include a.nsh                         # COMMENT
!include b.nsh                         ; COMMENT
