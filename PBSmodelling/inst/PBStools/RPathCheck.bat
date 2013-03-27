@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

call Rpaths.bat

if not exist "%TOOLS_PATH%\tar.exe" (
  ECHO Cannot find Tools - check TOOLS_PATH "%TOOLS_PATH%"
  set PBSERROR=1 )

rem if not exist "%PERL_PATH%\perl.exe" (
rem   ECHO Cannot find perl -  check PERL_PATH "%PERL_PATH%"
rem   set PBSERROR=1 )

if not exist "%R_PATH%\Rcmd.exe" (
  ECHO Cannot find R - check R_PATH "%R_PATH%"
  set PBSERROR=1 )

if not exist "%GCC_PATH%\gcc.exe" (
  ECHO Cannot find gcc - check GCC_PATH "%GCC_PATH%"
  set PBSERROR=1 )

rem if not exist "%MINGW_PATH%\gcc.exe" (
rem  ECHO Cannot find MinGW - check MINGW_PATH "%MINGW_PATH%"
rem   set PBSERROR=1 )

if not exist "%TEX_PATH%\latex.exe" (
  ECHO Cannot find latex - check TEX_PATH "%TEX_PATH%"
  set PBSERROR=1 )

rem if not exist "%HTMLHELP_PATH%\hhc.exe" (
rem   ECHO Cannot find HTML Help Workshop - check HTMLHELP_PATH "%HTMLHELP_PATH%"
rem   set PBSERROR=1 )

rem if not exist "%SVN_PATH%\svn.exe" (
rem    ECHO Cannot find svn - check SVN_PATH "%SVN_PATH%"
rem    set PBSERROR=1 )

rem set Path=.;%TOOLS_PATH%;%PERL_PATH%;%R_PATH%;%GCC_PATH%;%MINGW_PATH%;%TEX_PATH%;%HTMLHELP_PATH%;%SVN_PATH%
set Path=.;%TOOLS_PATH%;%R_PATH%;%GCC_PATH%;%TEX_PATH%
set TMPDIR=%TMP%
rem set HOMEDRIVE=C:
rem set HOMEPATH=\Documents and Settings\HaighR

if not defined PBSERROR (
  echo All program paths look good
  if not defined PBS_NO_PAUSE (
    pause
  )
)
if defined PBSERROR (
  pause )

