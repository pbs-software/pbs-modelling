@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

rem ***** Edit the six PATH variables listed below *****

set R_Root=C:\Program Files\R\R-2.15.2\
set TOOLS_PATH=C:\Rtools\bin
set PERL_PATH=C:\Rtools\perl\bin
set MINGW_PATH=C:\Rtools\MinGW\bin
set TEX_PATH=C:\Program Files\MiKTeX\miktex\bin
set HTMLHELP_PATH=C:\Program Files\HTML Help Workshop

set R_PATH=%R_Root%\bin\i386

