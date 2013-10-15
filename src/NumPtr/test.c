void test1(double * a, int n){
  int i;
  for (i=0; i<n ; i++){
  printf("%f\n",a[i]);
  }
}

void test2(double ** a, int n, int m){
  int i,j;
  for (i=0; i<n ; i++){
  for (j=0; j<m ; j++){
  printf("%i %i\t %f\n",i,j,a[i][j]);
  }
}
}

void test3(double *** a, int n, int m, int l){
  int i,j,k;
  for (i=0; i<n ; i++){
  for (j=0; j<m ; j++){
  for (k=0; k<l ; k++){
  printf("%i %i %i\t %f\n",i,j,k,a[i][j][k]);
  }
}
}
}
