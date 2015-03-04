#include <stdio.h>
#define N 10 
int main()
{
  int c, d;
  double x[N] = {3.1, -1.2, 5.3, 1, 4.4, 21, 3, 7, -1.2, 3.2};
  double t;
  for (c = 1 ; c <= N - 1; c++) {
    d = c;
 
    while ( d > 0 && x[d] < x[d-1]) {
      t          = x[d];
      x[d]   = x[d-1];
      x[d-1] = t;
 
      d--;
    }
  }
 
  printf("Sorted data:\n");
 
  for (c = 0; c <= N - 1; c++) {
    printf("%f ", x[c]);
  }
 printf("\n\nMedian of x is %f \n", (x[N/2-1] + x[N/2])/2);
  return 0;
}