#ifndef __RPS_H__
#define __RPS_H__

class Challenger;

class RPS {
  public:
    virtual int accept(Challenger& r) const = 0;
    virtual ~RPS() = 0;
};

class Rock: public RPS {
  public:
    int accept(Challenger& r) const override;
    ~Rock();
};

class Paper: public RPS {
  public:
    int accept(Challenger& r) const override;
    ~Paper();
};

class Scissors: public RPS {
  public:
    int accept(Challenger& r) const override;
    ~Scissors();
};

#endif
