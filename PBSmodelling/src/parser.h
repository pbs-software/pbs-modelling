/*=============================================================================
  Copyright (C) 2005-2019 Fisheries and Oceans Canada

  This file is part of PBS Modelling.

  PBS Modelling is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  PBS Modelling is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PBS Modelling; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
=============================================================================*/

#ifndef __PARSER_H_
#define __PARSER_H_

#include <R.h>
#include <Rinternals.h>

/*==========================================================
 Create a 2|3-element list (with names "key" (optional), 
   "value", "quoted")
   args:  key    - first element
          val    - second element
          quoted - third element: "Y" or "N"
          list_names - constant 2-length STRSXP for 
                       "key", "value" names
   returns: the SEXP of the list.
============================================================*/
SEXP addPair(char *key, char *val, char *quoted, SEXP list_names, SEXP list_names_value_only);


/*==========================================================
 Converts a single string into a list of (key, value) lists
   i.e. convert "nokey keyname=foo" into the following R list:
   list(list(value="nokey"), list(key="keyname", value="foo"))
   
   args: str - R character string
         env - R environment for use in calling R code
         fname - R character string of filename for use in error reporting
         lineNum - R numeric value for line number for use in error reporting
   
   return: R list of 1-element, or 2-element lists.
           i.e.: "nokey keyname=foo" becomes
           list(list(value="nokey"), list(key="keyname", value="foo"))
============================================================*/
SEXP strToList(SEXP str, SEXP env, SEXP fname, SEXP lineNum);


/*==========================================================
 Converts a single string into a vector of extracted values
   while treating spaces in quotes as regular chars
   
   args: str - R character string
         env - R environment for use in calling R code
         fname - R character string of filename for use in error reporting
         lineNum - R numeric value for line number for use in error reporting
	
	return: R vector of values
============================================================*/
SEXP strToVector(SEXP str, SEXP env, SEXP fname, SEXP lineNum);


/*==========================================================
 Returns a string without any leading #comments
 and strips off leftover whitespace on the right side
============================================================*/
SEXP stripComments(SEXP str);


#endif /* __PARSER_H_ */
