@rem Robocopy arguments used:
@rem Usage     :: ROBOCOPY source destination [file [file]...] [options]
@rem  /R:0     :: number of Retries on failed copies: default 1 million
@rem  /W:0     :: Wait time between retries: default is 30 seconds
@rem  /COPYALL :: COPY ALL file info (equivalent to /COPY:DATSOU)
@rem  /NP      :: No Progress - don't display percentage copied
@rem  /XJ      :: eXclude symbolic links (for both files and directories) and Junction points
@rem  /XO      :: eXclude Older files
@rem  /XX      :: eXclude eXtra files and directories
@rem  /XF file :: eXclude Files matching given names/paths/wildcards

Echo Off
rem Batch file to copy a package from a Local (LOC) or Subversion (SVN) trunk directory to %Build%

SETLOCAL EnableDelayedExpansion EnableExtensions

set mydir=C:\Users\haighr\Files
set logfile=%mydir%\Archive\Logs\svn2git.log
rem set copyprog=C:\Zaps\Rats\xxcopy\xxcopy.exe
set copyprog=C:\Windows\System32\Robocopy.exe

set buildDir=C:\Users\haighr\Files\Projects\R\Build
rem Check that %buildDir% exists
if not exist %buildDir%\NUL GOTO NoBuild
set buildProj=%buildDir%\%1
set githubDir=C:\Users\haighr\Files\Projects\GitHub

if "%1"=="" (
	ECHO ERROR - you must specify a package name
	ECHO example: %0 PBSmodelling SVN
	GOTO Exit
	) else (
	set package=%1
	)
if "%2"=="" (
	SET repotag=pbs
	) else (
	SET repotag=%2
	)
if "%3"=="" (
	SET repo=SVN
	) else (
	SET repo=%3
	)

if "%repo%"=="LOC" (
	call set sourceDir=C:\Users\haighr\Files\Projects\R\Source\Local
	if not exist %sourceDir%\NUL GOTO NoSource
	call set sourceProj=%%sourceDir%%\%package%
	)
if "%repo%"=="SVN" (
	call set sourceDir=C:\Users\haighr\Files\Projects\R\Source\pbs-software
	if not exist %sourceDir%\NUL GOTO NoSource
	set string=
	set string=%package%
	rem set sumting=(echo %package% | tr [:upper:] [:lower:])
	call :lenStr !string! nchar
	rem call set sourceProj=%%sourceDir%%\%%package:~0,3%%-%%package:~3,!nchar!%%\trunk\%package%
	rem call set sourceBase=%%sourceDir%%\%%package:~0,3%%-%%package:~3,!nchar!%%
	call set sourceProj=%%sourceDir%%\%repotag%-%%package:~3,!nchar!%%\trunk\%package%
	call set sourceBase=%%sourceDir%%\%repotag%-%%package:~3,!nchar!%%


	rem Might as well update the Github repository with new files
	if NOT "%package%"=="PBSvault" (
	rem if NOT ("%package%"=="PBSvault" OR "%package%"=="PBSstewdio") (
		rem call set githubProj=%githubDir%\%%package:~0,3%%-%%package:~3,!nchar!%%\%package%
		rem call set githubBase=%githubDir%\%%package:~0,3%%-%%package:~3,!nchar!%%
		call set githubProj=%githubDir%\%repotag%-%%package:~3,!nchar!%%\%package%
		call set githubBase=%githubDir%\%repotag%-%%package:~3,!nchar!%%
		rem call set repotag=%%package:~0,3%%
	)
)
del /s "%sourceProj%\*.bak"

echo %sourceDir% -- %sourceBase% -- %sourceProj% 
echo %githubDir% -- %gitHubBase% -- %githubProj%
echo %buildDir% -- %buildProj%
echo %repotag%
rem GOTO:eof

rem Check that the source directory exists
rem if not exist .\%1\NUL GOTO NoSource
if not exist %sourceProj%\NUL GOTO NoSource
rem cd /d %sourceDir%

rem Clear the destination directory if it exists
if exist %buildProj%.Rcheck\NUL (
  echo Removing destination %buildProj%.Rcheck
  rmdir /S %buildProj%.Rcheck
  rm -fv %1*.zip
)
rem GOTO:Exit
if exist %buildProj%\NUL (
  echo Removing destination %buildProj%
  rmdir /S %buildProj%
)

:OKdest
rem Hidden .svn directories will not get copied
rem %copyprog% /E /I %sourceProj% %buildProj%  & :: used for XXCOPY
rem copyprog%  %sourceProj%  %buildProj%  *.*  /R:0 /W:0 /COPYALL /NP /XJ /XO /XX  >> %logfile%
%copyprog%  %sourceProj%  %buildProj%  /MIR /R:0 /W:0  >> %logfile%

if "%repo%"=="LOC" GOTO copyBuild
if "%package%"=="PBSvault" GOTO copyBuild
:: if "%package%"=="PBSstewdio" GOTO copyBuild

if not exist %githubDir%\NUL GOTO copyBuild
echo ===============================================================================>> %logfile%
echo Copy %1 files from SVN to GIT repo, where>> %logfile%
echo SVN: %sourceProj%>> %logfile%
echo GIT: %githubProj%>> %logfile%
echo DATE: %date%  %time% >> %logfile%
echo.>> %logfile%
if NOT "%package%"=="PBSvault" (
	rem %copyprog% /C /D /H /R /S /TCC /Y /PJ0 /PZ0 /Q1 %sourceProj% %githubProj% >> %logfile%
	rem %copyprog% /C /D /R /S /TCC /Y /PJ0 /PZ0 /Q1 /X:trunk\ %sourceBase% %githubBase% >> %logfile%
	%copyprog%  %sourceProj%  %githubProj%  *.*  /R:0 /W:0 /E /NP /XJ /XO /XX  >> %logfile%
	%copyprog%  %sourceBase%  %githubBase%  *.*  /R:0 /W:0 /E /NP /XJ /XO /XX /XD trunk >> %logfile%
)
:copyBuild
echo Change to the destination for checking and building:
ENDLOCAL
set buildDir=C:\Users\haighr\Files\Projects\R\Build
cd /d %buildDir%
rem pushd %buildDir%
echo %buildDir%

GOTO Exit

:lenStr %string% nchar ::returns the length of a string minus 3 characters for 'PBS'
rem                    -- string  [in] - variable name containing the string being measured for length
rem                    -- nchar  [out] - variable to be used to return the length of the string
rem Based on 'revStr' from 'devcom': http://www.computerhope.com/forum/index.php?topic=85897.0
	SETLOCAL ENABLEDELAYEDEXPANSION
	rem set line=sdas sfsa fwfwa21321
	set nchar=0
	:LOOP
		call set tmpa=%%string:~!nchar!,1%%%
rem echo !tmpa!
		if not "!tmpa!" equ "" (
			rem set rline=!tmpa!%rline%
			set /a nchar+=1
rem echo !nchar!
			GOTO LOOP
		)
		set /a nchar-=3  :: PBS = 3 characters
rem echo %nchar%
	rem )
   ENDLOCAL & set /a "%~2"="%nchar%" :: seen outside as !nchar!
exit /b
	rem ( ENDLOCAL & REM RETURN VALUES
	rem 	IF "%~2" NEQ "" SET /a %~2=%nchar%
	rem )
	rem EXIT /b

:NoBuild
Echo You must designate a Build directory with the variable Build
GOTO Exit

:NoSource
Echo source project %sourceProj% not found
GOTO Exit

:Exit
set copyprog=
set sourceDir=
set sourceProj=
set sourceBase=
set buildDir=
set buildProj=
set githubDir=
set githubProj=
set githubBase=
set package=
set string=
set nchar=
set tmpa=
set repo=
set TEMP=
:eof

