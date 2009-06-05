@ECHO OFF
if not defined PBS_SETLOCAL (
	SETLOCAL
	SET PBS_SETLOCAL=1 )

if "%1"=="" (
	echo ERROR - you must specify a package name
	echo example: %0 PBSmodelling 65
	goto end )

if "%2"=="" (
	set page=1
	) else (
	set page=%2)

set dviP=.Rd2dvi$
set ext=aux;dvi;idx;ilg;ind;log;out;pdf;tex;toc
SET PBS_NO_PAUSE=1
call RPathCheck.bat

if not defined PBSERROR (
	for %%a in (%ext%) do (
		if exist "%1.%%a" (
			rm -f "%1.%%a" ) )
	if exist %dviP% (rm -rf %dviP%)
	sed 's/\${R_PAPERSIZE-a4}/letter/g' %R_PATH%\Rd2dvi.sh > temp.txt
	sed 's/begin{document}/begin{document}\n\\\\\\\setcounter{page}{%page%}/g' temp.txt > %R_PATH%\Rd2dvi4pbs.sh
	R CMD Rd2dvi4pbs.sh --pdf --no-clean %1
	cp %dviP%\Rd2.tex %1.tex
	rm -f temp.txt
)

:end