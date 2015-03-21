#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define N  150 /* number of observations */
#define P  2 /* number of predictors */

void dgesv_(int *NN, int *NRHS, double *A, int *LDA, int *IPIV,
            double *B, int *LDB, int *INFO);
int main(int argc, char * argv[]){
  /* longley dataset from R: Employed (Y) GNP.deflator and Population (X) */
    double Y[N];
double X[N][P];

double XtX1[(P+1)*(P+1)], XtX0[P*P]; 
double XtY1[P+1], XtY0[P];

int ipiv1[P+1], ipiv0[P];
int i=0, j, k, n1, n2, info;
int itc = 0;
FILE *f;
itc = atoi(argv[2]);
if (argc != 3){
    printf("This program have 2 arguments: data intercept \n");
    printf("data: data file\n");
    printf("intercept : 1 = intercept, 0 = no intercept\n");
    return 1;
  }
    

  f = fopen(argv[1], "r");
  while(fscanf(f, "%lf%lf%lf\n",
               &Y[i], X[i], X[i]+1)==3){
                i++;
               }
fclose(f);
printf("Sample size and number of predictors are %d and %d respectively.\n", N, P);
switch(itc){
  case 1:
  
XtX1[0] = N;

for(i = 1;i<P+1;i++){
  XtX1[i*(P+1) + 0] = 0;
  for(j=0;j<N;j++)
  XtX1[i*(P+1) + 0] += X[j][i-1];
  XtX1[0*(P+1)+i] = XtX1[i*(P+1) + 0];
}

for(i=1;i<P+1;i++){
  for(j=1;j<P+1;j++){
    XtX1[i*(P+1)+j] = 0;
    for(k=0;k<N;k++){
      XtX1[i*(P+1)+j]+=X[k][i-1]*X[k][j-1];
    }
  }
}

/* Calculate (1,X)'*Y which is a 3x1 matrix*/
XtY1[0] = 0;
for(i=0;i<N;i++)
XtY1[0] +=Y[i];

for(i=1;i<P+1;i++){
  XtY1[i] = 0;
  for(j=0;j<N;j++)
  XtY1[i] +=X[j][i-1]*Y[j];
}
n1 = P+1;
n2 = 1;
dgesv_(&n1, &n2, XtX1, &n1, ipiv1, XtY1, &n1, &info);

if (info != 0)
printf("dgesv error %d\n", info);
printf("The regression coefficients: ");
for (i=0; i<P+1; i++)
printf("%f ", XtY1[i]);
printf("\n");
break;

case 0:

for(i=0;i<P;i++){
  for(j=0;j<P;j++){
    XtX0[i*P+j] = 0;
    for(k=0;k<N;k++){
      XtX0[i*P+j]+=X[k][i]*X[k][j];
    }
  }
}

/* Calculate (1,X)'*Y which is a 3x1 matrix*/
for(i=0;i<P;i++){
  XtY0[i] = 0;
  for(j=0;j<N;j++)
  XtY0[i] +=X[j][i]*Y[j];
}
n1 = P;
n2 = 1;
dgesv_(&n1, &n2, XtX0, &n1, ipiv0, XtY0, &n1, &info);

if (info != 0)
printf("dgesv error %d\n", info);
printf("The regression coefficients: ");
for (i=0; i<P; i++)
printf("%f ", XtY0[i]);
printf("\n");
break;

}
/* Calculate (1, X)'*(1, X) which is a 3X3 matrix*/

return 0;
}
/* gcc -pedantic -Wall -ansi  hw33.c -llapack -lblas -lgfortran */
/* 26.851352  0.240842	0.119026*/