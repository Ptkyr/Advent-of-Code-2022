#ifndef __CHALLENGER_H__
#define __CHALLENGER_H__

class Rock;
class Paper;
class Scissors;

class Challenger {
  public:
    virtual int value() const = 0;
    virtual int challenge(const Rock& r) const = 0;
    virtual int challenge(const Paper& p) const = 0;
    virtual int challenge(const Scissors& s) const = 0;
    virtual ~Challenger() = default;
};

#endif
