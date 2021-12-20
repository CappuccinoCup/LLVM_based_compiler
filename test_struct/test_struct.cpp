#include <iostream>
  
extern "C" {
	  int isTriangle(struct Triangle*);
      double foo(int, struct TwoTriangles*);
}

struct Triangle {
    double a;
    double b;
    double c;
};

struct TwoTriangles {
    struct Triangle a;
    int b;
    struct Triangle c;
};

int main() {
    struct Triangle t1, t2, t3;
    t1.a = 2, t1.b = 3, t1.c = 3;
    t2.a = 3.3, t2.b = 3.3, t2.c = 3.3;
    t3.a = 2, t3.b = 5, t3.c = 7;

    std::cout << "t1 is a triangle: " << isTriangle(&t1) << std::endl;
    std::cout << "t2 is a triangle: " << isTriangle(&t2) << std::endl;
    std::cout << "t3 is a triangle: " << isTriangle(&t3) << std::endl;

    struct TwoTriangles tt1, tt2, tt3;
    tt1.a = t1, tt1.b = 1, tt1.c = t2;
    tt2.a = t2, tt2.b = 2, tt2.c = t3;
    tt3.a = t3, tt3.b = 3, tt3.c = t1;

    std::cout << "foo(1, tt1) is: " << foo(1, &tt1) << std::endl;
    std::cout << "foo(2, tt2) is: " << foo(2, &tt2) << std::endl;
    std::cout << "foo(3, tt3) is: " << foo(3, &tt3) << std::endl;
}
