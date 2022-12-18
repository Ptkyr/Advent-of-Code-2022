#include <iostream>
#include <fstream>
#include "guides.h"

int main() {
    std::ifstream input{"day2.txt"};
    int score = 0;
    int scoretwo = 0;
    std::string str;
    while (getline(input, str)) {
        score += GuideOne.at(str);
        scoretwo += GuideTwo.at(str);
    }
    std::cout << "Part A solution: " << score << std::endl;
    std::cout << "Part B solution: " << scoretwo << std::endl;
}
