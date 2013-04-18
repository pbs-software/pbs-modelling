Echo Off
Rem Batch file to copy a package from a Subversion (SVN) trunk directory to %Temp%
Rem An environment variable TEMP must designate a temporary directory

Rem Check that %Temp% exists
If !%Temp% == ! goto NoTemp

Rem Check that the source directory exists
if not exist .\%1\NUL goto NoSource

Rem Clear the destination directory if it exists
if not exist %Temp%\%1\NUL goto OKdest
Echo Removing destination %Temp%\%1
rmdir /S %Temp%\%1
if not exist %Temp%\%1.Rcheck\NUL goto OKdest
rmdir /S %Temp%\%1.Rcheck

:OKdest
Rem Hidden .svn directories won't get copied
xcopy /E /I %1 %Temp%\%1
Rem Change to the destination for checking and building
cd /d %Temp%
Goto Exit

:NoTemp
Echo You must designate a temporary directory with the variable TEMP
Goto Exit

:NoSource
Echo Source directory %1 not found
Goto Exit

:Exit
