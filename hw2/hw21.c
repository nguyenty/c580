#include <stdio.h>
#include <math.h>
#define P0 0.01 /* lower limit of the probability (p)*/
#define P1 0.5 /* upper limit of the probability (p)*/
#define PLEN 10 /* number of columns*/
#define N 5 /* number of experiments (n)*/


int fact(int n); /*funtion to calculate factorial of n*/

int main(){
  int x = 0; 
  float dist, p;
  p = P0;
  dist = (P1-P0)/(PLEN-1);
  printf("x\\p\t"); 
  for (p = P0; p <= P1; p+= dist){
    printf("%.4f ", p); /* print p values*/
  }
  printf("\n\v"); /* print vertical tab */
  for (x = 0; x <= N; x++){
    printf("%d  \t", x);
    for (p = P0; p <= P1; p+= dist){
      printf("%.4f ", fact(N)/(fact(x)*fact(N-x))*pow(p,x)*pow(1-p, N-x)); /*print probabilites*/
    }
    printf("\n");
  }
  return 0;
}

int fact(int n){
  int out = 1;
  if (n ==0) out = 1;
  while (n >= 1){
    out = out*n;
    n--;
  }
  return out;
}