rem : This version creates and runs a new bash file Rd2dvi4pbs.sh from Rd2dvi.sh.
rem : Note, only effective starting with R-2.8.0.
@ECHO OFF
if not defined PBS_SETLOCAL (
	SETLOCAL
	SET PBS_SETLOCAL=1 )

if "%1"=="" (
	echo ERROR - you must specify a package name
	echo example: %0 PBSmodelling 81
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
	sed 's/\${\$}\"/\$\"\nR_PAPERSIZE=%R_PAPERSIZE%\nR_OSTYPE=%R_OSTYPE%/g' %R_PATH%\Rd2dvi.sh > %R_PATH%\temp01.txt
	rem sed 's/\\\begin{document}/\\\topmargin -0.25in \\\oddsidemargin 0in \\\evensidemargin 0in\n\\\textheight 9in \\\textwidth 6.5in\n\\\begin{document}\n\\\setcounter{page}{%page%}/g' %dviP%\temp.txt > %1.tex
	sed 's/makeindex{}/makeindex{}\n\\\\\\\topmargin -0.25in \\\\\\\oddsidemargin 0in \\\\\\\evensidemargin 0in\n\\\\\\\textheight 9in \\\\\\\textwidth 6.5in/g' %R_PATH%\temp01.txt > %R_PATH%\temp02.txt
	sed 's/begin{document}/begin{document}\n\\\\\\\setcounter{page}{%page%}/g' %R_PATH%\temp02.txt > %R_PATH%\Rd2dvi4pbs.sh
	R CMD Rd2dvi4pbs.sh --pdf --no-clean --no-preview %1
	%wzzip% %1-Manual.zip %dviP%\Rd2.*
	cp %dviP%\Rd2.tex %1.tex
	rem rm -f temp.txt
)

:end