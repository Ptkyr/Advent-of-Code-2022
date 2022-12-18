#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

class ElfPair {
    int one_begin = 0;
    int one_end = 0;
    int two_begin = 0;
    int two_end = 0;
public:
    friend std::istream& operator>>(std::istream& in, ElfPair& ep) {
        char c;
        in >> ep.one_begin;
        in >> c;
        in >> ep.one_end;
        in >> c;
        in >> ep.two_begin;
        in >> c;
        in >> ep.two_end;
        return in;
    }
    bool contained() const {
        return (one_begin >= two_begin && one_end <= two_end) || 
               (two_begin >= one_begin && two_end <= one_end);
    }
    bool overlap() const {
        return (one_begin >= two_begin && one_begin <= two_end) ||
               (one_end >= two_begin && one_end <= two_end) ||
               (two_begin >= one_begin && two_begin <= one_end) ||
               (two_end >= one_begin && two_end <= one_end);
    }
};

int main() {
    std::ifstream input{"day4.txt"};
    ElfPair ep;
    int sum = 0;
    int p2sum = 0;
    while (input >> ep) {
        if (ep.contained()) ++sum;
        if (ep.overlap()) ++p2sum;
    }
    std::cout << sum << std::endl;
    std::cout << p2sum << std::endl;
}
