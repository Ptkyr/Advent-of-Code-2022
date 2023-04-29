#ifndef __PARTONE_H__
#define __PARTONE_H__

#include "challenger.h"

class RockOne: public Challenger {
  public:
    int value() const override {
        return 1;
    }
    int challenge(const Rock& r) const override;
    int challenge(const Paper& p) const override;
    int challenge(const Scissors& s) const override;
};

class PaperOne: public Challenger {
  public:
    int value() const override {
        return 2;
    }
    int challenge(const Rock& r) const override;
    int challenge(const Paper& p) const override;
    int challenge(const Scissors& s) const override;
};

class ScissorsOne: public Challenger {
  public:
    int value() const override {
        return 3;
    }
    int challenge(const Rock& r) const override;
    int challenge(const Paper& p) const override;
    int challenge(const Scissors& s) const override;
};

#endif
