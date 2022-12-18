#include <iostream>
#include <utility>
#include <sstream>
#include <fstream>
#include <string>
#include <vector>

class Elf {
    int calories = 0;
public:
    friend std::istream& operator>>(std::istream& in, Elf& e) {
        std::string s;
        while (getline(in, s)) {
            if (s == "") break;
            std::istringstream tmp{s};
            int n;
            tmp >> n;
            e.calories += n;
        }
        return in;
    }
    int getCalories() const {
        return calories;
    }
};

int main() {
    std::ifstream input{"day1.txt"};
    std::vector<Elf> elves;

    while (true) {
        Elf elf;
        input >> elf;
        if (!input) break;
        elves.emplace_back(std::move(elf));
    }

    int max = 0;
    int second = 0;
    int third = 0;
    for (const auto& n: elves) {
        const int cal = n.getCalories();
        if (cal > max) max = cal;
    }

    for (const auto& n: elves) {
        const int cal = n.getCalories();
        if (cal > second && cal != max) second = cal;
    }

    for (const auto& n: elves) {
        const int cal = n.getCalories();
        if (cal > third && cal != max && cal != second) third = cal;
    }

    std::cout << "Part A solution: " << max << std::endl;
    std::cout << "Part B solution: " << max + second + third << std::endl;
}
