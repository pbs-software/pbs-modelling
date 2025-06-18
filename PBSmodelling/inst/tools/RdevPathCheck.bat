@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

call RdevPaths.bat

if not exist "%TOOLS_PATH%\tar.exe" (
  ECHO Cannot find Tools - check TOOLS_PATH "%TOOLS_PATH%"
  set PBSERROR=1 )

if not exist "%R64_PATH%\Rcmd.exe" (
  ECHO Cannot find R - check R64_PATH "%R64_PATH%"
  set PBSERROR=1 )

rem if not exist "%GCC32_PATH%\gcc.exe" (
rem  ECHO Cannot find gcc - check GCC64_PATH "%GCC32_PATH%"
rem  set PBSERROR=1 )

if not exist "%GCC64_PATH%\g++.exe" (
rem if not exist "%GCC64_PATH%\mingw32-make.exe" (
  ECHO Cannot find g++ - check GCC64_PATH "%GCC64_PATH%"
rem   ECHO Try issuing command "pacman -Sy mingw-w64-x86_64-make"
  set PBSERROR=1 )

if not exist "%TEX_PATH%\latex.exe" (
  ECHO Cannot find latex - check TEX_PATH "%TEX_PATH%"
  set PBSERROR=1 )

rem if not exist "%PERL_PATH%\perl.exe" (
rem    ECHO Cannot find perl -  check PERL_PATH "%PERL_PATH%"
rem    set PBSERROR=1 )

rem if not exist "%PYTHON_PATH%\python.exe" (
rem    ECHO Cannot find python -  check PYTHON_PATH "%PYTHON_PATH%"
rem    set PBSERROR=1 )

rem if not exist "%HTMLHELP_PATH%\hhc.exe" (
rem   ECHO Cannot find HTML Help Workshop - check HTMLHELP_PATH "%HTMLHELP_PATH%"
rem   set PBSERROR=1 )

if not exist "%SVN_PATH%\svn.exe" (
   ECHO Cannot find svn - check SVN_PATH "%SVN_PATH%"
   set PBSERROR=1 )

rem if not exist "%QPDF_PATH%\qpdf.exe" (
rem   ECHO Cannot find qpdf - check QPDF_PATH "%QPDF_PATH%"
rem    set PBSERROR=1 )

if not exist "%GS_PATH%\gswin64c.exe" (
   ECHO Cannot find gswin64c - check GS_PATH "%GS_PATH%"
   set PBSERROR=1 )

rem if not exist "%XPDF_PATH%\pdfinfo.exe" (
rem    ECHO Cannot find pdfinfo - check XPDF_PATH "%XPDF_PATH%"
rem    set PBSERROR=1 )

rem if not exist "%ORA_PATH%\sqlplus.exe" (
rem    ECHO Cannot find sqlplus - check ORA_PATH "%ORA_PATH%"
rem    set PBSERROR=1 )

rem if not exist "%CURL_PATH%\curl.exe" (
rem    ECHO Cannot find curl - check CURL_PATH "%CURL_PATH%"
rem    set PBSERROR=1 )

SETLOCAL ENABLEDELAYEDEXPANSION

rem Check that R's NO RMAP is set to true
echo -------------------------
echo _R_CXX_USE_NO_REMAP_=%_R_CXX_USE_NO_REMAP_%
echo -------------------------

if "%1"=="" (
	SET arch=64
	) else (
	SET arch=%1
	)
if "%arch%"=="32" (
	SET R_PATH=%R32_PATH%
	SET GCC_PATH=%GCC32_PATH%
	) else (
	if "%arch%"=="64" (
		SET R_PATH=%R64_PATH%
		SET GCC_PATH=%GCC64_PATH%
		) else (
		ECHO Cannot set architecture %arch% -- specify 32 or 64 in second argument
		set PBSERROR=1
		)
	)
rem echo %PBSERROR%
rem echo %arch%

if not defined PBSERROR (
  rem set Path=.;%TOOLS_PATH%;!R_PATH!;!GCC_PATH!;%TEX_PATH%;%SVN_PATH%;%QPDF_PATH%;%GS_PATH%;%PERL_PATH%;%XPDF_PATH%;%PYTHON_PATH%;%ORA_PATH%;%BAT_PATH%
  set Path=.;%TOOLS_PATH%;!R_PATH!;!GCC_PATH!;%BAT_PATH%;%TEX_PATH%;%SVN_PATH%;%QPDF_PATH%;%GS_PATH%
  rem echo !Path!
  rem echo %R_GSCMD%
  set TMPDIR=%TMP%
  rem echo !TMPDIR!
  rem set HOMEDRIVE=C:
  rem set HOMEPATH=\Documents and Settings\HaighR
  echo All program paths look good
  if not defined PBS_NO_PAUSE (
    pause
  )
)
ENDLOCAL & set pPath=%Path% & set rPath=%R_Path%
rem echo !myPath!
if defined PBSERROR (
  pause )

