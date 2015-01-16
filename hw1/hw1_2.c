#include<stdio.h>
int main(){
 int c;
 c = getchar();
 while (c != EOF){
 while((c>='0')&&(c<='9')) {putchar(c);}
 c = getchar();
 }
 return 0;
}