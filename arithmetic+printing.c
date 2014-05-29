#include <iostream>
#include <stdio.h>
int main ()
{	
	using namespace std;
  double a = 0;
  for (int b=0; b<10;b++)
  {
    a = 22.0 / (b + 7.0);
	  cout << a << "\n";
  }
  
  for(int i=0;i<=100000000;++i) printf("\r[%3d%%]",i);
  printf("\n\a");
	
  return 0;
}
