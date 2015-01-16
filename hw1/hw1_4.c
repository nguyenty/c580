#include <stdio.h>
int main(){
  int count, c;
  count = 0;
  c = getchar();
  while (c!= EOF){
    count++;
  }
  printf("\nNumber of words :%d\n", count);
  return 0;
}