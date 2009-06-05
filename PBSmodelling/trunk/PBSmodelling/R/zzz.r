
.onLoad <- function(lib, pkg)
{
	library.dynam("PBSmodelling", pkg, lib)
	.initPBSoptions()
	cat("
PBS Modelling 2.10 -- Copyright (C) 2005-2009 Fisheries and Oceans Canada

A complete user guide 'PBSmodelling-UG.pdf' appears 
in the '.../library/PBSmodelling/doc' folder.

Built on May 14, 2009
Pacific Biological Station, Nanaimo

")
}
