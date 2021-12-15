#include <iostream>
  
extern "C" {
	int foo(char, double);
	double bar(int, char);
}

int main(){
	std::cout << "foo of ('a', 1.1) is :" << foo('a', 1.1) << std::endl;
	std::cout << "foo of ('b', 2.2) is :" << foo('b', 2.2) << std::endl;
	std::cout << "foo of ('c', 3.3) is :" << foo('c', 3.3) << std::endl;
	std::cout << "bar of (1, 'a') is :" << bar(1, 'a') << std::endl;
	std::cout << "bar of (2, 'b') is :" << bar(2, 'b') << std::endl;
	std::cout << "bar of (3, 'c') is :" << bar(3, 'c') << std::endl;
}

