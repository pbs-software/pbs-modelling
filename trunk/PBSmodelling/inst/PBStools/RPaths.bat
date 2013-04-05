@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the following five PATH variables *****

set R_Base=C:\Utils\R
set R_Root=%R_Base%\R-3.0.0
set TOOLS_PATH=%R_Base%\Rtools\bin
set GCC_PATH=%R_Base%\Rtools\gcc-4.6.3\bin
set TEX_PATH=C:\Utils\MiKTeX\miktex\bin

rem Additional non-essential paths (edit or disable)
rem Perl and HTML Help historically used
set PERL_PATH=%R_Base%\Rtools\perl\bin
set HTMLHELP_PATH=C:\Utils\HHW

set R_PATH=%R_Root%\bin\i386
set SVN_PATH=C:\Utils\CollabNetSVC

rem ***** Edit or disable environment variables below *****
set R_PAPERSIZE=letter
set R_OSTYPE=windows
set _R_BUILD_COMPACT_VIGNETTES_=yes
set R_QPDF=C:/Utils/R/Rtools/qpdf-2.3.1/bin/qpdf.exe
set R_GSCMD=C:/Utils/Ghostgum/gscript/gs8.54/bin/gswin32c.exe


