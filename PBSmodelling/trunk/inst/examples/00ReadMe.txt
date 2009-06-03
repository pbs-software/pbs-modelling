The files in this directory illustrate applications based on 
PBSmodelling and other libraries. Generally, each example contains 
documentation, R code, a window description file, and (if required) 
other supporting files.

An example named xxx typically has corresponding files xxxDoc.txt or 
xxxDoc.pdf (documentation), xxx.r (R code), and xxxWin.txt (a window 
description). In the GUI for each example, buttons labelled "Docs", "R 
Code", and "Window" open these files provided that suitable programs 
have been associated with the file extensions *.txt, *.pdf, and *.r. In 
particular, the Acrobat Reader must be installed for reading *.pdf 
files, and you may need to associate a text file editor with *.r. On 
some systems, it may be necessary to use the function setPBSext to 
define these associations, as discussed in Section 2.3 of the User's 
Guide.*

Use the function runExamples() to view all examples available in 
PBSmodelling. This procedure copies all relevant files to a temporary 
directory located on the path defined by the environment variable Temp. 
It then opens a window in which radio buttons allow you to select any 
particular case. Closing the menu window causes the temporary files and 
related data to be cleaned up, and returns to the initial working 
directory.

Section 5 of the User's Guide* describes some, but not all, of the 
examples here. Future versions of PBSmodelling will add new examples. If 
their documentation seems primitive at first, please wait for 
improvements in subsequent versions.

-------------------------------------------------------------------------

*Note: Please remember to consult the User's Guide contained in the file

  ...\library\PBSmodelling\PBSmodelling-UG.pdf,

where "..." denotes the path to your R installation.
