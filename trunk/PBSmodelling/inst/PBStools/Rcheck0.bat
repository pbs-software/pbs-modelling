@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

if "%1"=="" (
  echo ERROR - you must specify a package name
  echo example: %0 PBSmodelling
  goto end )

SET PBS_NO_PAUSE=1
call RPathCheck.bat

if not defined PBSERROR (
  rem echo %TMPDIR%
  R CMD check %1 )
  rem R CMD check --as-cran %1 )

:end
