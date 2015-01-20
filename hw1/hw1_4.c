#include <stdio.h>
#define IN 1
#define OUT 0
int main(){
  int nw, c, state;
  nw = 0;
  state = OUT;
  c = getchar();
  while (c!= EOF){
    if (c ==' '|| c == '\t' || c == '\n') {
      state = OUT;
      }
      else if (state == OUT) {
        state = IN;
        nw = nw +1;
      } 
      c = getchar();
  }
  printf("\nNumber of words :%d\n", nw);
  return 0;
}