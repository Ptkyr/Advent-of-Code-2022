#include <iostream>
#include <fstream>
#include <vector>
#include "rps.h"
#include "rpsfactory.h"
#include "p1.h"

int main() {
    std::ifstream input{"day2.txt"};
    char x, y;
    int score = 0;
    int scoretwo = 0;
    while (input >> x >> y) {
        RPSConvert tmp{x};
        PartOneConvert p1{y};
        PartTwoConvert p2{y};
        auto enemy = tmp.create();
        auto one = p1.create();
        auto two = p2.create(x);
        score += enemy->accept(*one);
        scoretwo += enemy->accept(*two);
    }
    std::cout << score << std::endl;
    std::cout << scoretwo << std::endl;
}
