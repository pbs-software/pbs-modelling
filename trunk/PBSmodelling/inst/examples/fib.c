void fibonacci(int *n, int *len, double *retArr) {
  double xa=0, xb=1, xn=-1;
  int i,j;
  
  /* iterative loop */
  for(i=0;i<=*n;i++) {

    /* initial conds: fib(0)=0, fib(1)=1 */
    if (i <= 1) { xn = i; }

    /* fib(n)=fib(n-1)+fib(n-2) */
    else {xn = xa+xb; xa=xb; xb=xn; }

    /* save results if iteration i is within the 
       range from n-len to n */
    j=i - *n + *len - 1;
    if (j>=0) retArr[j] = xn;

  } /* end loop */
} /* end function */

