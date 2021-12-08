#include <iostream>
  
extern "C" {
	  int factorial(int n);
      double pretendToDoSomething(int n);
}

// int factorial(int n) {
//     if (!n)
//         return 1;

//     int result = 0, a;
//     int result1 = 1, result2 = 1;
//     a = n;

//     while (!(a == 0)) {
//         result1 = result1 * a;
//         a = a - 1;
//     }

//     for (int b = n; b > 0; b = b - 1)
//         result2 = result2 * b;

//     if (result1 == result2)
//         result = result1;

//     return result;
// }

// double pretendToDoSomething(int n) {
//     double a, result;
//     a = n * 5 + n * 2;
//     result = a - n;
//     return result - 5 * n;
// }

int main(){
    for (int i = 0; i < 5; i++)
    {
        std::cout << "factorial of " << i << " is: " << pretendToDoSomething(factorial(i)) << std::endl;
    }
}
