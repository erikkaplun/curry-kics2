@echo off
REM Batch file to call the front end for Curry

REM The installation directory of KiCS2
set KICS2HOME=`echo KICS2HOME must be defined here!`

REM Set 'cymake' executable
set CYMAKE = %KICS2HOME%/bin/.local/cymake.exe

%CYMAKE% --extended %*
