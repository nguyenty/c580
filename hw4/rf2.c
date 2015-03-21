#include <stdio.h>
int main(int argc, char *argv[]){
  FILE *f;
  double x[150][2],  y[150];
  int i = 0, j;
  f = fopen(argv[1], "r");
  while(fscanf(f, "%lf%lf%lf\n",
               x[i], x[i]+1, &y[i])==3){
                i++;
               }
fclose(f);
for(j = 0; j<i;j++){
  printf("%.1f %.1f %.1f\n", *(x[j]), *(x[j]+1), y[j]);
  }
  printf("i = %d\n", i);
  return 0;
}