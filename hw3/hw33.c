#include <stdio.h>
#define N  16 /* number of observations */
#define P  2 /* number of predictors */

void dgesv_(int *NN, int *NRHS, double *A, int *LDA, int *IPIV,
            double *B, int *LDB, int *INFO);
int main(){
  /* longley dataset from R: Employed (Y) GNP.deflator and Population (X) */
    double Y[N] = {60.323,61.122,60.171,61.187,63.221,63.639,64.989,
                   63.761,66.019,67.857,68.169,66.513,68.655,69.564,
                   69.331,70.551};
double X[N][P] =
{{83,107.608},
{88.5,108.632},
{88.2,109.773},
{89.5,110.929},
{96.2,112.075},
{98.1,113.27},
{99,115.094},
{100,116.219},
{101.2,117.388},
{104.6,118.734},
{108.4,120.445},
{110.8,121.95},
{112.6,123.366},
{114.2,125.368},
{115.7,127.852},
{116.9,130.081}};

double XtX[(P+1)*(P+1)]; 
double XtY[P+1];

int ipiv[P+1];
int i, j, k, n1, n2, info;
/* Calculate (1, X)'*(1, X) which is a 3X3 matrix*/
XtX[0] = N;

for(i = 1;i<P+1;i++){
  XtX[i*(P+1) + 0] = 0;
  for(j=0;j<N;j++)
  XtX[i*(P+1) + 0] += X[j][i-1];
  XtX[0*(P+1)+i] = XtX[i*(P+1) + 0];
}

for(i=1;i<P+1;i++){
  for(j=1;j<P+1;j++){
    XtX[i*(P+1)+j] = 0;
    for(k=0;k<N;k++){
      XtX[i*(P+1)+j]+=X[k][i-1]*X[k][j-1];
    }
  }
}

/* Calculate (1,X)'*Y which is a 3x1 matrix*/
XtY[0] = 0;
for(i=0;i<N;i++)
XtY[0] +=Y[i];

for(i=1;i<P+1;i++){
  XtY[i] = 0;
  for(j=0;j<N;j++)
  XtY[i] +=X[j][i-1]*Y[j];
}
n1 = P+1;
n2 = 1;
dgesv_(&n1, &n2, XtX, &n1, ipiv, XtY, &n1, &info);

if (info != 0)
printf("dgesv error %d\n", info);
for (i=0; i<P+1; i++)
printf("%f\t", XtY[i]);
printf("\n");
return 0;
}
/* gcc -pedantic -Wall -ansi  hw33.c -llapack -lblas -lgfortran */
/* 26.851352  0.240842	0.119026*/