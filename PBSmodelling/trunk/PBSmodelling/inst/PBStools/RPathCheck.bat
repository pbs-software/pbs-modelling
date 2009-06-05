@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

call Rpaths.bat

if not exist "%TOOLS_PATH%\tar.exe" (
  ECHO Can't find Tools - check TOOLS_PATH "%TOOLS_PATH%"
  set PBSERROR=1 )

if not exist "%PERL_PATH%\perl.exe" (
  ECHO Can't find perl -  check PERL_PATH "%PERL_PATH%"
  set PBSERROR=1 )

if not exist "%R_PATH%\Rcmd.exe" (
  ECHO Can't find R - check R_PATH "%R_PATH%"
  set PBSERROR=1 )

if not exist "%MINGW_PATH%\gcc.exe" (
  ECHO Can't find MinGW - check MINGW_PATH "%MINGW_PATH%"
  set PBSERROR=1 )

if not exist "%TEX_PATH%\latex.exe" (
  ECHO Can't find latex - check TEX_PATH "%TEX_PATH%"
  set PBSERROR=1 )

if not exist "%HTMLHELP_PATH%\hhc.exe" (
  ECHO Can't find HTML Help Workshop - check HTMLHELP_PATH "%HTMLHELP_PATH%"
  set PBSERROR=1 )

set Path=.;%TOOLS_PATH%;%PERL_PATH%;%R_PATH%;%MINGW_PATH%;%TEX_PATH%;%HTMLHELP_PATH%
set TMPDIR=%TMP%

if not defined PBSERROR (
  echo All program paths look good
  if not defined PBS_NO_PAUSE (
    pause
  )
)
if defined PBSERROR (
  pause )
