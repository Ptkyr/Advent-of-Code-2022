#include "rpsfactory.h"
#include "rps.h"
#include "p1.h"

RPSConvert::RPSConvert(const char& c): x{c} {}

RPSConvert::~RPSConvert() = default;

std::unique_ptr<RPS> RPSConvert::create() const {
    switch (x) {
        case 'A':
            return std::make_unique<Rock>();
        case 'B':
            return std::make_unique<Paper>();
        case 'C':
            return std::make_unique<Scissors>();
        default:
            throw "Invalid char";
    }
}

PartOneConvert::PartOneConvert(const char& c): x{c} {}

PartOneConvert::~PartOneConvert() = default;

std::unique_ptr<Challenger> PartOneConvert::create() const {
    switch (x) {
        case 'X':
            return std::make_unique<RockOne>();
        case 'Y':
            return std::make_unique<PaperOne>();
        case 'Z':
            return std::make_unique<ScissorsOne>();
        default:
            throw "Invalid char";
    }
}

// Cyclic helper function
static char lose(const char& c) {
    if (c == 'A') {
        return 'B';
    } else if (c == 'B') {
        return 'C';
    } else {
        return 'A';
    }
}

static char abctoxyz(const char& c) {
    switch (c) {
        case 'A':
            return 'X';
        case 'B':
            return 'Y';
        case 'C':
            return 'Z';
        default:
            throw "Invalid char";
    }
}

PartTwoConvert::PartTwoConvert(const char& c): x{c} {}

PartTwoConvert::~PartTwoConvert() = default;

std::unique_ptr<Challenger> PartTwoConvert::create(const char& c) const {
    switch (x) {
        case 'X':
            return PartOneConvert{abctoxyz(lose(lose(c)))}.create();
        case 'Y':
            return PartOneConvert{abctoxyz(c)}.create();
        case 'Z':
            return PartOneConvert{abctoxyz(lose(c))}.create();
        default:
            throw "Invalid char";
    }
}
