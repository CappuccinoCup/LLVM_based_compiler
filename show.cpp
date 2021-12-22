#include <iostream>

using namespace std;

extern "C" {
    double condition(int n);
	int loop();
    int loopEX();
}

int main() {
    cout << endl;
    
    cout << "condition(0) is " << condition(0) << endl;
    cout << endl;

    cout << "condition(1) is " << condition(1) << endl;
    cout << endl;

    cout << "loop() is: " << loop() << endl;
    cout << endl;

    cout << "loopEX() is: " << loopEX() << endl;
    cout << endl;
}