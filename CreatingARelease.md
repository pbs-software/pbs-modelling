# Introduction #

A growing list of manual tasks required before releasing to CRAN


# Details #

  * Modify Description for new release version
  * Update change log - ideally cross reference user reported bugs fixes with issue tickets
  * Update testWidgets.Rd with new content from showArgs()
  * Update user guide pdf
    * convert PBSmodelling-UG.doc to PBSmodelling-UG.pdf
    * RmakePDF.bat PBSmodelling p ( where p is the page number of the last page in PBSmodelling-UG.pdf)
    * remove first (blank) page of PBSmodelling.pdf (created by RmakePDF)
    * append PBSmodelling.pdf on to PBSmodelling-UG.pdf
  * create a SVN tag once tar.gz is uploaded to cran FTP