@ECHO OFF
if not defined PBS_SETLOCAL (
  SETLOCAL
  SET PBS_SETLOCAL=1 )

SET PBS_NO_PAUSE=1
call RPathCheck.bat

Start %R_Root%\doc\html\index.html

:end
