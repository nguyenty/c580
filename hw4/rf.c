#include <stdio.h>
int main(int argc, char *argv[]){
  FILE *f;
  double a, b, c, x1[150], x2[150], y[150];
  int i = 0, j;
  f = fopen(argv[1], "r");
  while(fscanf(f, "%lf%lf%lf\n",
               &a, &b, &c)==3){
                x1[i] = a;
                x2[i] = b;
                y[i] = c;
                i++;
               }
fclose(f);
for(j = 0; j<i;j++){
  printf("%.1f %.1f %.1f\n", x1[j], x2[j], y[j]);
  }
  printf("i = %d\n", i);
  return 0;
}