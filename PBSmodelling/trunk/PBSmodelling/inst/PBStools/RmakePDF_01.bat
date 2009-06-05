@ECHO OFF
if not defined PBS_SETLOCAL (
	SETLOCAL
	SET PBS_SETLOCAL=1 )

if "%1"=="" (
	echo ERROR - you must specify a package name
	echo example: %0 PBSmodelling
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
	R CMD Rd2dvi --pdf --no-clean %1 
	sed 's/a4paper/letter/g' %dviP%\Rd2.tex > %dviP%\temp.txt
	sed 's/begin{document}/begin{document}\n\\\setcounter{page}{%page%}/g' %dviP%\temp.txt > %1.tex
	latex %1
	latex %1
	makeindex %1
	pdflatex %1
)

:end