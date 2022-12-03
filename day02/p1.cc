#include "p1.h"

int RockOne::challenge(const Rock& r) const {
    return 3;
}

int RockOne::challenge(const Paper& p) const {
    return 0;
}

int RockOne::challenge(const Scissors& s) const {
    return 6;
}

int PaperOne::challenge(const Rock& r) const {
    return 6;
}

int PaperOne::challenge(const Paper& p) const {
    return 3;
}

int PaperOne::challenge(const Scissors& s) const {
    return 0;
}

int ScissorsOne::challenge(const Rock& r) const {
    return 0;
}

int ScissorsOne::challenge(const Paper& p) const {
    return 6;
}

int ScissorsOne::challenge(const Scissors& s) const {
    return 3;
}
