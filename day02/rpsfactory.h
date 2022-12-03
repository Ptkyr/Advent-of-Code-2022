#include <memory>

class RPS;
class Challenger;

class RPSConvert {
    const char x;
  public:
    RPSConvert(const char& c);
    ~RPSConvert();

    std::unique_ptr<RPS> create() const;
};

class PartOneConvert {
    const char x;
  public:
    PartOneConvert(const char& c);
    ~PartOneConvert();

    std::unique_ptr<Challenger> create() const;
};

class PartTwoConvert {
    const char x;
  public:
    PartTwoConvert(const char& c);
    ~PartTwoConvert();

    std::unique_ptr<Challenger> create(const char& c) const;
};
