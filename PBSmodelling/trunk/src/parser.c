#include <R.h>
#include <Rdefines.h>

#define MAXSTRSIZE 4*1024

/* dispError - passes an error message to .catError2
   equivalent to the R code:
          .catError2(error, fname, lineNum)
   args: error - string
         env - R environment where .catError2 is contained
         fname - filename of source code that had error
         lineNum - line number where error first occured
*/ 
void dispError(char *error, SEXP env, SEXP fname, SEXP lineNum)
{
	SEXP callStr,t,p;
	int objs=0;
	
	/*create list for func call*/
	PROTECT(t = callStr = allocList(4));
	objs++;
	SET_TYPEOF(callStr, LANGSXP);
	
	/*first element - function name*/
	SETCAR(t, install(".catError2"));
	
	/*create first arg - error message*/
	PROTECT(p=allocVector(STRSXP, 1));
    objs++;
    SET_STRING_ELT(p, 0, mkChar(error));
	t=CDR(t);
	SETCAR(t, p);
	
	/*2nd arg - fname*/
	t=CDR(t);
	SETCAR(t, fname);
	
	/*3rd arg - line number*/
	t=CDR(t);
	SETCAR(t, lineNum);

	/*preform the call*/
	eval(callStr, env);
    UNPROTECT(objs);
}


/* create a 2 element list (with names "key", "value")
   args:  key - first element
          val - second element
          *objs - count number of protected objects (pass by ref)
          list_names - constant 2 length STRSXP for "key", "value" names
   returns: the SEXP of the list
*/
SEXP addPair(char *key, char *val, int *objs, SEXP list_names, SEXP list_names_value_only)
{
	SEXP list, p;
	
	if (!key[0]) { /*no key was given (i.e. "")*/
		/*create a 1 element list*/
	    PROTECT(list = allocVector(VECSXP, 1));
	    (*objs)++;
	    
	    /*create a single string and attach to 0th element of list*/
	    PROTECT(p=allocVector(STRSXP, 1));
	    (*objs)++;
	    SET_STRING_ELT(p, 0, mkChar(val));
	    SET_VECTOR_ELT(list, 0, p);

	    /*name them "key", and "value"*/
	    setAttrib(list, R_NamesSymbol, list_names_value_only);
	    return list;
	}
	/*otherwise save both key and value*/

	/*create a 2 element list*/
    PROTECT(list = allocVector(VECSXP, 2));
    (*objs)++;
    
    /*create a single string and attach as 0th element of list*/
    PROTECT(p=allocVector(STRSXP, 1));
    (*objs)++;
    SET_STRING_ELT(p, 0, mkChar(key));
    SET_VECTOR_ELT(list, 0, p);
    
    /*create a single string and attach to 1st element of list*/
    PROTECT(p=allocVector(STRSXP, 1));
    (*objs)++;
    SET_STRING_ELT(p, 0, mkChar(val));
    SET_VECTOR_ELT(list, 1, p);
    
    /*name them "key", and "value"*/
    setAttrib(list, R_NamesSymbol, list_names);
    
    return list;
}


/* countVals - calculate the number of values given in a string
   args: s - string to count
   return: count of values (to be used in allocating list size)
*/
int countVals(const char *s) {
	int escape=0, quoted=0, quotefound=0, i=0, kvpair=0, charfound=0;
	char c;
    
    for(i=0;s[i];i++) {
    	c = s[i];
    	
    	if (escape!=0) {
    		escape=0;
    		continue;
    	}
    	if (c=='\\') {
    		escape=1;
    		continue;
    	}
    	if (c=='"') {
    		quoted=!quoted; /*toggle quote state*/
    		quotefound=1;
    		continue;
    	}
    	if (c=='#' && !quoted) {
    		break;
    	}
    	if (c==' ' || c=='\t') {
    		if (quoted) {
    			continue;
    		}
    		/*save key and vals*/
    		if (charfound || quotefound)
    			kvpair++;
    		/*reset word count*/
    		charfound=0;
    		quotefound=0;
    		continue;
    	}
    	charfound=1;
    }
    if (charfound || quotefound)
    	kvpair++; /*last word isnt caught by loop*/
    
	return kvpair;
}


/* converts a single string into a list of (key, value) lists
   i.e. convert "nokey keyname=foo" into the following R list:
   list(list(value="nokey"), list(key="keyname", value="foo"))
   
   args: str - R character string
         env - R environment for use in calling R code
         fname - R character string of filename for use in error reporting
         lineNum - R numeric value for line number for use in error reporting
   
   return: R list of 1-element, or 2-element lists.
           i.e.: "nokey keyname=foo" becomes
           list(list(value="nokey"), list(key="keyname", value="foo"))
*/
SEXP strToList(SEXP str, SEXP env, SEXP fname, SEXP lineNum)
{
	SEXP list, list_names, list_names_value_only, p;
	int objs=0, escape, quoted, equal, quotefound, j, i, kvpair;
	char *key, *value, buf1[MAXSTRSIZE+1], buf2[MAXSTRSIZE+1], c;
	const char *s;
    
    /*make a char vector for "key", "value" names for list pairs*/
    PROTECT(list_names = allocVector(STRSXP, 2));
    objs++;
    SET_STRING_ELT(list_names, 0,  mkChar("key"));
    SET_STRING_ELT(list_names, 1,  mkChar("value"));

    /*make a char vector for only "value" name for list pairs with no valid key*/
    PROTECT(list_names_value_only = allocVector(STRSXP, 1));
    objs++;
    SET_STRING_ELT(list_names_value_only, 0,  mkChar("value"));


    PROTECT(str = AS_CHARACTER(str));
    objs++;
    s = CHAR(STRING_ELT(str,0));
    kvpair=0;
    escape=0;
    quoted=0;
    quotefound=0;
    equal=0;
    j=0;
    
    key=buf1;
    value=buf2;
    
    /*make master list for the right length*/
    i=countVals(s);
    PROTECT(list = allocVector(VECSXP, i));
    objs++;
    
    for(i=0;s[i];i++) {
    	c = s[i];
    	if (j>=MAXSTRSIZE) {
    		list=R_NilValue;
    		dispError("Maximum string length exceeded", env, fname, lineNum);
			goto strToList_exit;
    	}
    	if (escape!=0) {
    		value[j++]=c;
    		escape=0;
    		continue;
    	}
    	if (c=='\\') { /*in R code - it had to also be in quotes, is this really the case?*/
    		value[j++]=c; /*don't strip them out yet - it might be a "character" or "characterVector"*/
    		escape=1;
    		continue;
    	}
    	if (c=='"') {
    		quoted=!quoted; /*toggle quote state*/
    		quotefound=1;
    		continue;
    	}
    	if (c=='\'') {
    		if (quoted) {
    			value[j++]=c;
    			continue;
    		}
    		/*error - single quotes must be escaped*/
    		list=R_NilValue;
    		dispError("unexpected single quote found", env, fname, lineNum);
			goto strToList_exit;
    	}
    	if (c=='#' && !quoted) {
    		break;
    	}
    	if (c==' ' || c=='\t') {
    		if (quoted) {
    			value[j++]=c;
    			continue;
    		}
    		/*save key and vals
    		  DON'T FORGET ABOUT CODE JUST BELOW LOOP */
    		if (j || quotefound) {
    			value[j]='\0';
    			if (!equal)
    				key="";
    			p=addPair(key, value, &objs, list_names, list_names_value_only);
    			SET_VECTOR_ELT(list, kvpair++, p);
    		}
    		else if (equal) {
	    		list=R_NilValue;
	    		dispError("key found, but no value given - use key=\"\" instead of key=", env, fname, lineNum);
				goto strToList_exit;
    		}
    		
    		/*reset counts*/
    		j=0;
    		quotefound=0;
    		equal=0;
    		key=buf1;
    		value=buf2;
    		
    		continue;
    	}
    	if (c=='=' && !quoted && !equal) {
    		if (!j) {
    			/*found "=value"*/
	    		list=R_NilValue;
	    		dispError("unexpected '=' found", env, fname, lineNum);
				goto strToList_exit;
    		}
    		equal=1;
    		value[j]='\0';
    		key=value;
    		value=buf1;
    		j=0;
    		
    		continue;
    	}
    	/*if no cases were found, then treat as a regular char*/
    	value[j++]=c;
    }
    if (quoted) {
    	list=R_NilValue;
    	dispError("closing doublequote is missing", env, fname, lineNum);
		goto strToList_exit;
    }
    /*save key and vals
      DON'T FORGET ABOUT ABOVE CODE IN LOOP*/
    if (j || quotefound) {
	    value[j]='\0';
	    if (!equal)
	    	key="";
	    p=addPair(key, value, &objs, list_names, list_names_value_only);
	    SET_VECTOR_ELT(list, kvpair++, p);
	}
    else if (equal) {
		list=R_NilValue;
		dispError("key found, but no value given - use key=\"\" instead of key=", env, fname, lineNum);
		goto strToList_exit;
    }
strToList_exit:

    UNPROTECT(objs);
    return(list);
}


/* converts a single string into a vector of extracted values
   while treating spaces in quotes as regular chars
   
   args: str - R character string
         env - R environment for use in calling R code
         fname - R character string of filename for use in error reporting
         lineNum - R numeric value for line number for use in error reporting
	
	return: R vector of values
*/
SEXP strToVector(SEXP str, SEXP env, SEXP fname, SEXP lineNum)
{
	SEXP list;
	int objs=0, escape, quoted, quotefound, j, i, vecIndex;
	char value[MAXSTRSIZE+1], c;
	const char *s;


    PROTECT(str = AS_CHARACTER(str));
    objs++;
    s = CHAR(STRING_ELT(str,0));
    vecIndex=0;
    escape=0;
    quoted=0;
    quotefound=0;
    j=0;
    
    
    /*make master list for the right length*/
    i=countVals(s);
    PROTECT(list = allocVector(STRSXP, i));
    objs++;
    
    for(i=0;s[i];i++) {
    	c = s[i];
    	if (j>=MAXSTRSIZE) {
    		list=R_NilValue;
    		dispError("Maximum string length exceeded", env, fname, lineNum);
			goto strToVector_exit;
    	}
    	if (escape!=0) {
    		switch(c) {
    			case 'n':
    				c='\n';
    				break;
    			case 't':
    				c='\t';
    				break;
    			case 'r':
    				c='\r';
    				break;
    		}
    		value[j++]=c;
    		escape=0;
    		continue;
    	}
    	if (c=='\\' && quoted) {
    		escape=1;
    		continue;
    	}
    	if (c=='"') {
    		quoted=!quoted; /*toggle quote state*/
    		quotefound=1;
    		continue;
    	}
    	if (c=='#' && !quoted) {
    		break;
    	}
    	if (c==' ' || c=='\t') {
    		if (quoted) {
    			value[j++]=c;
    			continue;
    		}
    		/*save vals*/
    		if (j || quotefound) {
    			value[j]='\0';
    			SET_STRING_ELT(list, vecIndex++, mkChar(value));
    		}
    		
    		/*reset counts*/
    		j=0;
    		quotefound=0;
    		
    		continue;
    	}
    	/*if no cases were found, then treat as a regular char*/
    	value[j++]=c;
    }
    if (quoted) {
    	list=R_NilValue;
    	dispError("closing doublequote is missing", env, fname, lineNum);
		goto strToVector_exit;
    }
    if (j || quotefound) {
    	/*save vals*/
    	value[j]='\0';
    	SET_STRING_ELT(list, vecIndex++, mkChar(value));
	}
    
strToVector_exit:

    UNPROTECT(objs);
    return(list);
}


/*returns a string without any leading #comments
 *and strips off leftover whitespace on the right side */
SEXP stripComments(SEXP str)
{
	int objs=0, escape, quoted, i, last_whitspace, escaped_whitespace;
	int first_whitespace=0, non_white_found=0;
	char c;
	char s[strlen(CHAR(STRING_ELT(str,0)))+1];
	strcpy( s, CHAR(STRING_ELT(str,0)) );

    PROTECT(str = AS_CHARACTER(str));
    objs++;
    /*s = CHAR(STRING_ELT(str,0));*/

    escape=0;
    quoted=0;
    last_whitspace=-1;
    escaped_whitespace=0;
    
    for(i=0;s[i];i++) {
    	c = s[i];
    	if (escape!=0) {
    		if (c==' ' || c=='\t') {
    			last_whitspace=i;
    			escaped_whitespace=1;
    		}
    		escape=0;
    		continue;
    	}
    	if (c=='\\') {
    		escape=1;
    		last_whitspace=-1; /*invalidate whitespace*/
    		escaped_whitespace=0;
    		non_white_found=1;
    		continue;
    	}
    	if (c=='"') {
    		quoted=!quoted; /*toggle quote state*/
    		last_whitspace=-1; /*invalidate whitespace*/
    		escaped_whitespace=0;
    		non_white_found=1;
    		continue;
    	}
    	if (c=='#' && (!quoted || escaped_whitespace)) {
    		/*found where comment starts*/
    		if (i>0) {
    			if (last_whitspace>=0 && (s[i-1]==' ' || s[i-1]=='\t'))
    				i=last_whitspace;
    		}
    		s[i]='\0';
    		SET_STRING_ELT(str, 0, mkChar(s));
    		break;
    	}
    	if (c==' ' || c=='\t') {
    		if (last_whitspace==-1)
    			last_whitspace=i;
    		if (!non_white_found)
    			first_whitespace++;
    		continue;
    	}
    	/*a normal char was found - invalidate last whitespace*/
    	last_whitspace=-1;
    	escaped_whitespace=0;
    	non_white_found=1;
    }
    /*no # was found - but there might be whitespace*/
    if (last_whitspace>=0)
		s[last_whitspace]='\0';
	SET_STRING_ELT(str, 0, mkChar(s+first_whitespace));
	
    UNPROTECT(objs);
    return(str);
}
