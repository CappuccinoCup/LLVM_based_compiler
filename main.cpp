#include <iostream>
  
extern "C" {
	  int foo(int, double);
}

int main(){
	std::cout << "foo of 1, 4.2 is :" << foo(1,4.2) << std::endl;
	std::cout << "foo of 3, 4.0 is :" << foo(3,4.0) << std::endl;
}

