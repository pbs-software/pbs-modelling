void sum(double *a, int *n) {
  int i; double total=0;

  for(i=0;i<n[0];i++) total += a[i];
  /* return the total as the first element of a */
  a[0]=total; 
}
