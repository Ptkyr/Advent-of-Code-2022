#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

class Crates {
    std::vector<std::vector<char>> crates {
        {'G', 'D', 'V', 'Z', 'J', 'S', 'B'},
        {'Z', 'S', 'M', 'G', 'V', 'P'},
        {'C', 'L', 'B', 'S', 'W', 'T', 'Q', 'F'},
        {'H', 'J', 'G', 'W', 'M', 'R', 'V', 'Q'},
        {'C', 'L', 'S', 'N', 'F', 'M', 'D'},
        {'R', 'G', 'C', 'D'},
        {'H', 'G', 'T', 'R', 'J', 'D', 'S', 'Q'},
        {'P', 'F', 'V'},
        {'D', 'R', 'S', 'T', 'J'}
    };
public:
    void p1rearrange(const int from, const int to) {
        crates[to - 1].emplace_back(crates[from - 1].back());
        crates[from - 1].pop_back();
    }
    void p1process(const int mult, const int from, const int to) {
        for (int i = 0; i < mult; ++i) {
            p1rearrange(from, to);
        }
    }
    void p2process(const int mult, const int from, const int to) {
        std::vector<char>& start = crates[from - 1];
        std::vector<char>& dest = crates[to - 1];
        for (int i = mult; i > 0; --i) {
            dest.emplace_back(start[start.size() - i]);
        }
        for (int j = 0; j < mult; ++j) {
            start.pop_back();
        }
    }
    friend std::ostream& operator<<(std::ostream& out, const Crates& c) {
        for (int i = 0; i < 9; ++i) {
            out << c.crates[i].back();
        }
        return out;
    }
};

int main() {
    std::ifstream input{"day5.txt"};
    std::string s;
    Crates meme;
    Crates two;
    while (getline(input, s)) {
        int mult, from, to;
        std::istringstream ss{s};
        ss >> s;
        ss >> mult;
        ss >> s;
        ss >> from;
        ss >> s;
        ss >> to;
        meme.p1process(mult, from, to);
        two.p2process(mult, from, to);
    }
    std::cout << "Part 1: " << meme << std::endl;
    std::cout << "Part 2: " << two << std::endl;
}
