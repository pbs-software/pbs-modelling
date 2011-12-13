.onLoad <- function(libname, pkgname)
{
	library.dynam("PBSmodelling", pkgname, libname)
	#.initPBSoptions()
	
	pkg_info <- utils::sessionInfo( package="PBSmodelling" )$otherPkgs$PBSmodelling
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- "unkown"
	
	userguide_path <- system.file( "doc/PBSmodelling-UG.pdf", package = "PBSmodelling" )
	
	packageStartupMessage("
-----------------------------------------------------------
PBS Modelling ", pkg_info$Version, " -- Copyright (C) 2005-2011 Fisheries and Oceans Canada

A complete user guide 'PBSmodelling-UG.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo
-----------------------------------------------------------


")
	#Load custom PBSmodelling tcl scripts
	tcl("lappend", "auto_path", system.file( "tcl_scripts", package = "PBSmodelling" ) )
	tclRequire( "PBSmodelling" )

	#TO DO find a better place
	bwidget <- tclRequire("BWidget", warn = FALSE)
	tktable <- tclRequire("Tktable")
	if( is.logical( bwidget ) || is.logical( tktable ) ) {
		packageStartupMessage("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" )
	}
	if( is.logical( bwidget ) ) {
		#try included distribution
		tcl("lappend", "auto_path", system.file( "thirdparty/BWidget-1.9.0/", package = "PBSmodelling" ) )
		bwidget <- tclRequire("BWidget")
		if( is.logical( bwidget ) ) {
		packageStartupMessage(
			"ERROR: PBSmodelling requires the tcl package \"BWidget\"\nand cannot proceed until it is installed.\n" ,
			"Ubuntu (apt) users can install via the command:\n\tsudo apt-get install bwidget\n",
			"Bwidget source can be downloaded from\n\thttp://sourceforge.net/projects/tcllib/files/\n")
		}
	}
	if( is.logical( tktable ) ) {
		packageStartupMessage(
			"ERROR: PBSmodelling requires the tcl package \"Tktable\"\nand cannot proceed until it is installed.\n",
			"Ubuntu (apt) users can install via the command:\n\tsudo apt-get install libtktable2.10\n",
			"Mac (port) users can install via the command:\n\tsudo port install tktable\n",
			"assuming Mac Ports is installed (http://www.macports.org/)\n",
			"tktable can also be donwloaded from \n\thttp://sourceforge.net/projects/tktable/files/\n" )
	}
	if( is.logical( bwidget ) || is.logical( tktable ) ) {
		packageStartupMessage("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
	}
}

.onAttach <- function(libname, pkgname){
	.initPBSoptions()
}

