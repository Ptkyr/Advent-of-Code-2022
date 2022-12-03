#include "p1.h"

int RockTwo::challenge(const Rock& r) const {
    return 3;
}

int RockTwo::challenge(const Paper& p) const {
    return 0;
}

int RockTwo::challenge(const Scissors& s) const {
    return 6;
}

int PaperTwo::challenge(const Rock& r) const {
    return 6;
}

int PaperTwo::challenge(const Paper& p) const {
    return 3;
}

int PaperTwo::challenge(const Scissors& s) const {
    return 0;
}

int ScissorsTwo::challenge(const Rock& r) const {
    return 0;
}

int ScissorsTwo::challenge(const Paper& p) const {
    return 6;
}

int ScissorsTwo::challenge(const Scissors& s) const {
    return 3;
}
