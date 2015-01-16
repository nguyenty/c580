#include <stdio.h>
#define lower 0.0 /* this is a float variable */
#define upper 200.0 // how to open file *.out
#define nrow 201
int main() {
  double f, c, step;
  step = (upper - lower)/nrow; // 
  f = lower; 
  printf("F\t C\n");
  while (f <= upper){
    c = 5*(f - 32)/9;
    printf("%.2f \t %.2f\n", f, c);
    f = f + step;
  }
  return 1;
}