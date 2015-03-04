#include <stdio.h>
#include<time.h>
#define MATLIB_STANDALONE
#include<Rmath.h>
int main(){
  int i; 
  set_seed(time(NULL), 580580)/*initialize, set seed, from Rmath.d*/
  for(i = 1; i <= 10; i++){
    printf("%f", unif_rand())
  }
  return 0;
}