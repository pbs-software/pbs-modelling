
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	
	pkg_info <- sessionInfo( package="PBSmodelling" )$otherPkgs$PBSmodelling
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
	tclRequire("BWidget")
	tclRequire("Tktable")
}
