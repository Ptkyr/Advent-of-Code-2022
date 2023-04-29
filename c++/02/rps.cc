#include "rps.h"
#include "p1.h"

RPS::~RPS() = default;

int Rock::accept(Challenger& r) const {
    return r.value() + r.challenge(*this);
}

Rock::~Rock() = default;

int Paper::accept(Challenger& r) const {
    return r.value() + r.challenge(*this);
}

Paper::~Paper() = default;

int Scissors::accept(Challenger& r) const {
    return r.value() + r.challenge(*this);
}

Scissors::~Scissors() = default;
