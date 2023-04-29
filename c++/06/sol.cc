#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

template<int N>
class Buffer {
    int char_count = 0;
    std::vector<char> buf;

    void slide() {
        for (int i = 0; i < N - 1; ++i) {
            buf[i] = buf[i + 1];
        }
        ++char_count;
    }
    bool packet() const {
        if (char_count < N) return false;
        for (int i = 0; i < N; ++i) {
            for (int j = i + 1; j < N; ++j) {
                if (buf[i] == buf[j]) return false;
            }
        }
        return true;
    }
public:
    Buffer(): buf(N, ' ') {}

    int getCount() const {
        return char_count;
    }

    friend std::istream& operator>>(std::istream& in, Buffer& b) {
        char c;
        while (in >> c) {
            b.slide();
            b.buf[N - 1] = c;
            if (b.packet()) break;
        }
        return in;
    }
};

int main() {
    std::ifstream input{"day6.txt"};
    Buffer<4> b;
    input >> b;
    std::ifstream input2{"day6.txt"};
    Buffer<14> b2;
    input2 >> b2;
    std::cout << "Part 1: " << b.getCount() << std::endl;
    std::cout << "Part 2: " << b2.getCount() << std::endl;
}
