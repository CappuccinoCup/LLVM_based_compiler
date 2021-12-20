#include <iostream>
  
extern "C" {
	  int isTriangle(Triangle);
}

struct Triangle {
    double a;
    double b;
    double c;
};

// int isTriangle(Triangle t) {
//     return t.a < t.b + t.c & t.b < t.a + t.c & t.c < t.a + t.b;
// }

int main() {
    Triangle t1, t2, t3;
    t1.a = 2, t1.b = 3, t1.c = 3;
    t2.a = 3.3, t2.b = 3.3, t2.c = 3.3;
    t3.a = 2, t3.b = 5, t3.c = 7;

    std::cout << "t1 is a triangle: " << isTriangle(t1) << std::endl;
    std::cout << "t2 is a triangle: " << isTriangle(t2) << std::endl;
    std::cout << "t3 is a triangle: " << isTriangle(t3) << std::endl;
}
