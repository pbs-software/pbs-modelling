@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the seven PATH variables listed below *****
set R_Base=C:\WinApps\R
set R_Root=%R_Base%\R-2.13.1
set TOOLS_PATH=%R_Base%\Rtools\bin
set PERL_PATH=%R_Base%\Rtools\perl\bin
set MINGW_PATH=%R_Base%\Rtools\MinGW\bin
set TEX_PATH=C:\WinApps\MiKTeX\miktex\bin
set HTMLHELP_PATH=C:\Utils\HHW

set R_PATH=%R_Root%\bin\i386
set perlR=%R_Root%\share\perl\R

rem ***** Edit environment variables below *****
set R_PAPERSIZE=letter
set R_OSTYPE=windows

rem ***** Specify special programs if you use them *****
set xcopy=%windir%\system32\xcopy.exe
rem set wzzip=C:\WinApps\WinZip\wzzip.exe

