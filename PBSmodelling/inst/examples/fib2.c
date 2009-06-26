#include <R.h>
#include <Rdefines.h>

SEXP fib2(SEXP sexp_n, SEXP sexp_len) {
	SEXP retVals; /* ptr to output R vector we will create */
	double *p_retVals, xa=0, xb=1, xn;
	int n, len, i, j;
	
	/* convert R variables into C 'int's */
	len = INTEGER_VALUE(sexp_len);
	n = INTEGER_VALUE(sexp_n);
	
	/* Allocate space for an R vector */
	PROTECT(retVals = NEW_NUMERIC(len));
	p_retVals = NUMERIC_POINTER(retVals);
	

	for(i=0;i<=n;i++) {
		/* init conds: fib(0)=0, fib(1)=1 */
		if (i <= 1) {
			xn = i;
		}
		/* fib(n)=fib(n-1)+fib(n-2) */
		else {
			xn = xa+xb;
			xa = xb;
			xb = xn;
		}
		/* save results if iteration i is within the 
		   range from n-len to n */
		j=i - n + len - 1;
		if (j>=0)
			p_retVals[j] = xn;
	}
	
	UNPROTECT(1);
	return retVals;
}

