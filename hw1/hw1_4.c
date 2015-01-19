#include <stdio.h>
int main(){
  int count, c;
  count = 0;
  c = getchar();
  while (c!= EOF){
    if (c ==' ') count = count + 1; /*this program only counts words in a line where two consecutive words are apart one tab*/
    c = getchar();
  }
  printf("\nNumber of words :%d\n", count);
  return 0;
}