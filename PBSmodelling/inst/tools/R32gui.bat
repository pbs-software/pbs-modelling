@ECHO OFF
rem SETLOCAL ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION
SETLOCAL ENABLEDELAYEDEXPANSION

if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

SET PBS_NO_PAUSE=1
call RPathCheck.bat 32

rem Removing all spaces (not just leading and trailing) can be done using 'set var=%var: =%'
set Path=!pPath: =!;C:\WINDOWS\system32;
set R_Path=!rPath!

rem Need to supplement the Path defined in RPathCheck.bat with whatever you need in R
rem set Path=%Path%;%SystemRoot%\system32;
rem (temp) set Path=%Path%;C:\Apps\Ora81\bin;%SystemRoot%\system32

:Start
set Ffile=C:\\Apps\\R\\.First.r

:R
cd /d .
If exist .RData (
	rem setlocal EnableDelayedExpansion
	rem set filedatetime=%date%
	rem set filedatetime=%filedatetime:~12,2%%filedatetime:~4,2%%filedatetime:~7,2%
	rem mv -f .RData .RData.{%filedatetime%}
	mv -f .RData .RData.old
)
%R_PATH%\R.exe CMD BATCH %Ffile%

Start %R_PATH%\Rgui.exe

