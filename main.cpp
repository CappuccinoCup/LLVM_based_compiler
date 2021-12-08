#include <iostream>
  
extern "C" {
	  int fib(int);
}

int main(){
	std::cout << "fib of 1 is :" << fib(1) << std::endl;
	std::cout << "fib of 2 is :" << fib(2) << std::endl;
	std::cout << "fib of 3 is :" << fib(3) << std::endl;
	std::cout << "fib of 4 is :" << fib(4) << std::endl;
	std::cout << "fib of 5 is :" << fib(5) << std::endl;
	std::cout << "fib of 6 is :" << fib(6) << std::endl;
	std::cout << "fib of 7 is :" << fib(7) << std::endl;
}

