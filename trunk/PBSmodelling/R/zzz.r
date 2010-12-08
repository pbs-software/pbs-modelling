
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	
	pkg_info <- utils::sessionInfo( package="PBSmodelling" )$otherPkgs$PBSmodelling
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- "unkown"
	
	userguide_path <- system.file( "doc/PBSmodelling-UG.pdf", package = "PBSmodelling" )
	
	cat("
PBS Modelling", pkg_info$Version, "-- Copyright (C) 2005-2010 Fisheries and Oceans Canada

A complete user guide 'PBSmodelling-UG.pdf' is located at 
", userguide_path, "

Packaged on", pkg_date, "
Pacific Biological Station, Nanaimo

")
	#Load custom PBSmodelling tcl scripts
	tcl("lappend", "auto_path", system.file( "tcl_scripts", package = "PBSmodelling" ) )
	tclRequire( "PBSmodelling" )

	#TODO find a better place
	bwidget <- tclRequire("BWidget", warn = FALSE)
	tktable <- tclRequire("Tktable")
	if( is.logical( bwidget ) || is.logical( tktable ) ) {
		cat( "-------------------------------------------------------------\n" )
	}
	if( is.logical( bwidget ) ) {
		#try included distribution
		tcl("lappend", "auto_path", system.file( "thirdparty/BWidget-1.9.0/", package = "PBSmodelling" ) )
		bwidget <- tclRequire("BWidget")
		if( is.logical( bwidget ) ) {
			cat( "ERROR: PBS Modelling requires the tcl package \"BWidget\"\nand cannot proceed until it is installed.\n" )
			cat( "Ubuntu (apt) users can install via the command:\n\tsudo apt-get install bwidget\n" )
			cat( "Bwidget source can be downloaded from http://sourceforge.net/projects/tcllib/files/\n\n" )
		}
	}
	if( is.logical( tktable ) ) {
		cat( "ERROR: PBS Modelling requires the tcl package \"Tktable\"\nand cannot proceed until it is installed.\n" )
		cat( "Ubuntu (apt) users can install via the command:\n\tsudo apt-get install libtktable2.9\n" )
		cat( "Mac (port) users can install via the command:\n\tsudo port install tktable (assuming mac ports is installed)\n" )
		cat( "tktable can also be donwloaded from http://sourceforge.net/projects/tktable/files/\n\n" )
	}
	if( is.logical( bwidget ) || is.logical( tktable ) ) {
		cat( "-------------------------------------------------------------\n" )
	}
}
