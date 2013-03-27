@ECHO OFF

if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

if "%1"=="" (
  echo ERROR - you must specify a package name
  echo example: %0 PBSmodelling
  goto end )

if "%2"=="" (
	set aliens=100
	) else (
	set aliens=%2)

SET PBS_NO_PAUSE=1
call RPathCheck.bat

if not defined PBSERROR (
	R CMD INSTALL --build --compact-docs --compile-both %1
	rem R CMD INSTALL --build --resave-data --compact-docs %1
	rem R CMD INSTALL --build %1
	rem Rcmd build  --binary %1  ::deprecated
  )

:end