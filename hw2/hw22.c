#include <stdio.h>
#include <time.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>
int main() {
  double u, x;
  set_seed(time(NULL), 580580); /* set seed */
    u = unif_rand(); /* uniform random variable */
    x = -log(1- (1-exp(-2))*u);
    printf("%f\n ", x);
    return 0;
    }