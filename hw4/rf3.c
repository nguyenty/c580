#include <stdio.h>
int main(){
  int x[2][3] = {{1, 2, 3}, {10, 20, 30}};
  int i, j;
  for(i = 0; (i<2); i++){
    for(j = 0; j<3; j++){
      printf("%p  %p\n", &x[i][j], (x[i]+j));
    }
  }
  return 0;
  
  }