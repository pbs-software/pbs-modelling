@ rem %R_PATH% is set in RPaths.bat
@ rem Individual Rd files:   %R_PATH%\R CMD Rd2dvi --pdf PBSmodelling.Rd
@ rem package documentation: %R_PATH%\R CMD Rd2dvi --pdf PBSmodelling

@ECHO OFF
if not defined PBS_SETLOCAL (
	SETLOCAL
	SET PBS_SETLOCAL=1 )

if "%1"=="" (
	ECHO ERROR - you must specify a package name
	ECHO example: %0 PBSmodelling 79
	goto end )

if "%2"=="" (
	SET page=1
	) else (
	SET page=%2)

SET ext=aux;dvi;idx;ilg;ind;log;out;pdf;tex;toc
SET PBS_NO_PAUSE=1
CALL RPathCheck.bat
if exist %R_Root%\share\texmf\tex\latex\Rd.sty cp -f -v %R_Root%\share\texmf\tex\latex\Rd.sty .
echo %R_PATH%
rem goto :exit
SETLOCAL ENABLEDELAYEDEXPANSION

if not defined PBSERROR (
	for %%a in (%ext%) do (
		if exist "%1.%%a" (
			rm -f "%1.%%a" ) )
	rm -f -r .Rd2pdf* rem remove all the temporary R directories
 
	%R_PATH%\R CMD Rd2pdf --no-clean --no-preview  %1
	dir /b .Rd2pdf* > dirRd.txt
	set dviP=
	for /f %%i in (dirRd.txt) do ( rem should only see one temporary directory
		set dviP=
		set dviP=!dviP!%%i
		echo !dviP! 
		sed 's/makeindex{}/makeindex{}\n\\\topmargin -0.25in \\\oddsidemargin 0in \\\evensidemargin 0in\n\\\textheight 9in \\\textwidth 6.5in/g' !dviP!\Rd2.tex > Rd2a.tex
		sed 's/begin{document}/begin{document}\n\\\setcounter{page}{%page%}/g' Rd2a.tex > %1.tex
	)
	latex -interaction=nonstopmode %1.tex
	makeindex %1.idx
	latex -interaction=nonstopmode %1.tex
	makeindex %1.idx
	latex -interaction=nonstopmode %1.tex
	latex -interaction=nonstopmode %1.tex
	dvips -q %1.dvi
	ps2pdf %1.ps
)
:exit
SET ext=
SET PBS_NO_PAUSE=
SET dviP=
:end
