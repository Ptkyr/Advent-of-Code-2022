#ifndef __PARTTWO_H__
#define __PARTTWO_H__

#include "challenger.h"

class RockTwo: public Challenger {
  public:
    int value() const override {
        return 1;
    }
    int challenge(const Rock& r) const override;
    int challenge(const Paper& p) const override;
    int challenge(const Scissors& s) const override;
};

class PaperTwo: public Challenger {
  public:
    int value() const override {
        return 2;
    }
    int challenge(const Rock& r) const override;
    int challenge(const Paper& p) const override;
    int challenge(const Scissors& s) const override;
};

class ScissorsTwo: public Challenger {
  public:
    int value() const override {
        return 3;
    }
    int challenge(const Rock& r) const override;
    int challenge(const Paper& p) const override;
    int challenge(const Scissors& s) const override;
};

#endif
