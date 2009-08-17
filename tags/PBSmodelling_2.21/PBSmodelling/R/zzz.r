
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	
	pkg_info <- utils::sessionInfo( package="PBSmodelling" )$otherPkgs$PBSmodelling
	pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	
	userguide_path <- system.file( "doc/PBSmodelling-UG.pdf", package = "PBSmodelling" )
	
	cat("
PBS Modelling", pkg_info$Version, "-- Copyright (C) 2005-2009 Fisheries and Oceans Canada

A complete user guide 'PBSmodelling-UG.pdf' is located at 
", userguide_path, "

Packaged on", pkg_date, "
Pacific Biological Station, Nanaimo

")

	#TODO find a better place
	bwidget <- tclRequire("BWidget")
	tktable <- tclRequire("Tktable")
	if( is.logical( bwidget ) || is.logical( tktable ) ) {
		cat( "-------------------------------------------------------------\n" )
	}
	if( is.logical( bwidget ) ) {
		cat( "ERROR: PBS Modelling requires the tcl package \"BWidget\"\nand cannot proceed until it is installed.\n" )
		cat( "Ubuntu (apt) users can install via the command:\n\tapt-get install bwidget\n\n" )
	}
	if( is.logical( tktable ) ) {
		cat( "ERROR: PBS Modelling requires the tcl package \"Tktable\"\nand cannot proceed until it is installed.\n" )
		cat( "Ubuntu (apt) users can install via the command:\n\tapt-get install libtktable2.9\n\n" )
	}
	if( is.logical( bwidget ) || is.logical( tktable ) ) {
		cat( "-------------------------------------------------------------\n" )
	}
}
