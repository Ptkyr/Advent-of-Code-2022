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
    std::string s;
    int sum = 0;
    while (getline(input, s)) {
        const int pack_size = s.length() / 2;
        std::string s1 = s.substr(0, pack_size);
        std::string s2 = s.substr(pack_size);
        for (int i = 0; s1[i]; ++i) {
            if (s2.find(s1[i]) != std::string::npos) {
                sum += prio(s1[i]);
                break;
            }
        }
    }
    std::cout << sum << std::endl;
}
