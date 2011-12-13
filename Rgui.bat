@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

SET PBS_NO_PAUSE=1
call RPathCheck.bat

rem Need to supplement the Path defined in RPathCheck.bat with whatever you need in R
set Path=%Path%;C:\Apps\Ora81\bin;%SystemRoot%\system32

:Start
rem  set Rbin=C:\Apps\R\R2130\bin\i386
set Ffile=C:\\Apps\\R\\.First.r

:R
  If not exist .RData (
    %R_PATH%\R.exe CMD BATCH %Ffile%
  )
Start %R_PATH%\Rgui.exe

