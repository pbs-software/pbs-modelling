@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the PATH variables listed below *****

set R_Base=C:\Utils\R
set R_Root=%R_Base%\R-3.4.1
set TOOLS_PATH=%R_Base%\Rtools\bin
set GCC_PATH=%R_Base%\Rtools\gcc-4.6.3\bin
rem rem set GCC_PATH=%R_Base%\Rtools\mingw_64\bin
set TEX_PATH=C:\Utils\MiKTeX\miktex\bin

rem Additional non-essential paths (edit or disable)
rem Perl and HTML Help historically used
set PERL_PATH=C:\Utils\Perl\bin
set QPDF_PATH=C:\Utils\qpdf-5.1.2\bin
set GS_PATH=C:\Utils\Ghostscript\gs9.15\bin

set R_PATH=%R_Root%\bin\i386
set R32_PATH=%R_Root%\bin\i386
set R64_PATH=%R_Root%\bin\x64
set SVN_PATH=C:\Utils\CollabNetSVC

rem ***** Edit or disable environment variables below *****
set R_PAPERSIZE=letter
set R_OSTYPE=windows
set _R_BUILD_COMPACT_VIGNETTES_=yes
set R_QPDF=%QPDF_PATH%\qpdf.exe
set R_GSCMD=%GS_PATH%\gswin64c.exe


