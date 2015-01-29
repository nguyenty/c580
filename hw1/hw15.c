#include<stdio.h>
int main(){
  int i, j, c;
  double a, b;
  a =  1+2/5.0;
  printf ("%f\n",a); /*1.400000*/
  b = 1/2 + 3.0/4.0;
  printf ("%f\n",b); /*0.750000*/
  c = 3 < 10 && 2 * 8 < 2;
  printf("%d\n", c); /* 0 */
  i = 0;
  j = 2;
  j+=i++;
  printf("i = %d, j = %d\n", i, j); /* i = 1, j = 2*/
  return 0;
}