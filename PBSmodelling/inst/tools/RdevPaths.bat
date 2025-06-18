@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the PATH variables listed below *****

set R_Base=C:\Apps\R
set R_Root=%R_Base%\Rdev64
set R_Tools=%R_Base%\Rtools45
rem set R_Utils=%R_Base%\Rtools
set TOOLS_PATH=%R_Tools%\usr\bin
rem set GCC32_PATH=%R_Tools%\mingw32\bin
set GCC64_PATH=%R_Tools%\x86_64-w64-mingw32.static.posix\bin
set TEX_PATH=C:\Apps\MiKTeX\miktex\bin\x64
set GS_PATH=C:\Apps\gs\gs9.53.3\bin
rem set QPDF_PATH=%R_Utils%\qpdf-10.0.4\bin

rem set PERL_PATH=C:\Apps\Perl\bin
rem set PYTHON_PATH=C:\Apps\Python\P332
rem set HTMLHELP_PATH=C:\Apps\HHW
rem set CURL_PATH=%R_Utils%\curl-7.40.0-win64\bin

rem set R32_PATH=%R_Root%\bin\i386
set R64_PATH=%R_Root%\bin\x64
set R_PATH=%R_Root%\bin\x64
set SVN_PATH=C:\Apps\TortoiseSVN\bin

rem set ORA_PATH=C:\Apps\Oracle11\Home\BIN
rem set perlR=%R_Root%\share\perl\R
rem set XPDF_PATH=C:\Apps\R\xpdfbin-win-3.03\bin32

rem ***** Edit environment variables below *****
rem set xcopy=%windir%\system32\xxcopy.exe
rem set wzzip=C:\Apps\WinZip\wzzip.exe
set R_PAPERSIZE=letter
set R_OSTYPE=windows
set _R_BUILD_COMPACT_VIGNETTES_=yes
set _R_NO_S_TYPEDEFS_=true
set _R_CXX_USE_NO_REMAP_=true
rem set R_QPDF=%QPDF_PATH%\qpdf.exe
set R_GSCMD=%GS_PATH%\gswin64c.exe
set HOME=C:\Users\haighr\Files
set R_TEMP=%HOME%\Temp\R
rem ---- RH BAT DIR ----
set BAT_PATH=%HOME%\Archive\Bat

