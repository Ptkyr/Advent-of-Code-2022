#include <iostream>
#include <fstream>
#include <sstream>

int prio(const char& c) {
    if (c >= 'a' && c <= 'z') {
        return c - 96;
    } else if (c >= 'A' && c <= 'Z') {
        return c - 38;
    }
}

int main() {
    std::ifstream input{"day3.txt"};
    std::string one;
    std::string two;
    std::string three;
    int sum = 0;
    while (getline(input, one)) {
        getline(input, two);
        getline(input, three);
        for (int i = 0; one[i]; ++i) {
            if (two.find(one[i]) != std::string::npos &&
                three.find(one[i]) != std::string::npos) {
                sum += prio(one[i]);
                break;
            }
        }
    }
    std::cout << sum << std::endl;
}
