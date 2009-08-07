@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

if "%1"=="" (
  echo ERROR - you must specify a package name
  echo example: %0 PBSmodelling
  goto end )

if not exist %1.tar.gz (
  echo ERROR - file %1.tar.gz does not exist
  goto end )

SET PBS_NO_PAUSE=1
call RPathCheck.bat

if not defined PBSERROR (
  tar -xzvf %1.tar.gz )

:end