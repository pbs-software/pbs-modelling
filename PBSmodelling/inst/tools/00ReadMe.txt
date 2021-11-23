The batch files in this directory can facilitate R package development 
under Windows. For more information on building packages, see apendix B 
of the PBS Modelling User Guide*.

Revised batch files are included here:
-------------------------------------
Rcopy.bat         copy developer project to build directory for checking
RPaths.bat        specify local paths for various programs
RPathCheck.bat    check local paths by looking for key programs
Rcheck.bat        check the source code for errors (w/ CRAN check)
Rcheck0.bat       check the source code for errors (w/out CRAN check)
Rbuild.bat        build the source code into a binary Windows package
Rpack.bat         pack the source code into a source package
Runpack.bat       unpack source code into a package directory tree
RmakePDF.bat      make a PDF from the Rd files

(NOTE: `PBStry' very outdated and likely dysfunctional.)
You can use the small package archived in PBStry_x.xx.tar.gz as a 
convenient prototype for starting your own new package. It includes 
functions with C code called by the R function .C().
-------------------------------------------------------------------------
*Note: Please remember to consult the User's Guide contained in the file
  .../library/PBSmodelling/doc/PBSmodelling-UG.pdf,
where "..." denotes the path to your R installation.
