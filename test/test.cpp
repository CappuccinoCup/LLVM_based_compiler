#include <iostream>
  
extern "C" {
	  int factorial(int n);
      double pretendToDoSomething(int n);
}

int main(){
    for (int i = 0; i < 5; i++)
    {
        std::cout << "factorial of " << i << " is: " << pretendToDoSomething(factorial(i)) << std::endl;
    }
}
