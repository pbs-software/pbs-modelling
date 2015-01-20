Echo Off
rem Batch file to copy a package from a Local (LOC) or Subversion (SVN) trunk directory to %Build%

set BuildDir=C:\Users\username\Files\Projects\R\Build
rem Check that %BuildDir% exists
if not exist %BuildDir%\NUL goto NoBuild

if "%1"=="" (
	ECHO ERROR - you must specify a package name
	ECHO example: %0 PBSmodelling SVN
	goto end 
	) else (
	set package=%1
	)
rem echo %package%

if "%2"=="" (
	SET repo=SVN
	) else (
	SET repo=%2
	)
rem echo %repo%

if "%repo%"=="LOC" (
	call set Source=C:\Users\username\Files\Projects\R\Source\Local
	)
if "%repo%"=="SVN" (
	set string=
	set string=%package%
rem echo !string!
	call :lenStr !string! nchar
	call set Source=C:\Users\username\Files\Projects\R\Source\Google\%%package:~0,3%%-%%package:~3,!nchar!%%\trunk
	)
rem echo %Source%
rem echo %BuildDir%
rem goto:exit

rem Check that the source directory exists
rem if not exist .\%1\NUL goto NoSource
if not exist %Source%\%1\NUL goto NoSource
rem cd /d %Source%

rem Clear the destination directory if it exists
if not exist %BuildDir%\%1\NUL goto OKdest
Echo Removing destination %BuildDir%\%1
rmdir /S %BuildDir%\%1
if not exist %BuildDir%\%1.Rcheck\NUL goto OKdest
rmdir /S %BuildDir%\%1.Rcheck
rm -fv %1*.zip

:OKdest
rem Hidden .svn directories will not get copied
xcopy /E /I %Source%\%1 %BuildDir%\%1

Rem Change to the destination for checking and building
cd /d %BuildDir%

Goto:Exit

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
			goto LOOP
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
Goto Exit

:NoSource
Echo Source directory %Source%\%1 not found
Goto Exit

:Exit
set Source=
set BuildDir=
set package=
set string=
set nchar=
set tmpa=
set repo=
set TEMP=



