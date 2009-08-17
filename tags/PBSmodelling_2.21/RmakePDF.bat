rem : This version creates and runs a new bash file Rd2dvi4pbs.sh from Rd2dvi.sh.
rem : Note, only effective starting with R-2.8.0.
@ECHO OFF
if not defined PBS_SETLOCAL (
	SETLOCAL
	SET PBS_SETLOCAL=1 )

if "%1"=="" (
	echo ERROR - you must specify a package name
	echo example: %0 PBSmodelling 79
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
	sed 's/\${\$}\"/\$\"\nR_PAPERSIZE=%R_PAPERSIZE%\nR_OSTYPE=%R_OSTYPE%/g' %R_PATH%\Rd2dvi.sh > %R_PATH%\Rd2dvi4pbs.sh
	R CMD Rd2dvi4pbs.sh --pdf --no-clean --no-preview %1
	sed 's/makeindex{}/makeindex{}\n\\\topmargin -0.25in \\\oddsidemargin 0in \\\evensidemargin 0in\n\\\textheight 9in \\\textwidth 6.5in/g' %dviP%\Rd2.tex > %dviP%\temp01.tex
	sed 's/begin{document}/begin{document}\n\\\setcounter{page}{%page%}/g' %dviP%\temp01.tex > %dviP%\%1.tex
	
	latex %dviP%\%1 -output-directory=%dviP%
	latex %dviP%\%1 -output-directory=%dviP%
	makeindex %dviP%\%1
	pdflatex %dviP%\%1 -output-directory=%dviP%

	%wzzip% %1-Manual.zip %dviP%\%1.*
	cp -f %dviP%\%1.pdf %1.pdf
	rem rm -f temp.txt
)

:end