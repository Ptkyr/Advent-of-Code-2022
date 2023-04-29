#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <memory>
#include <algorithm>

class Entry {
    std::string name;
public:
    Entry(const std::string& s): name{s} {}
    std::string getName() const {
        return name;
    }
    virtual int size() const = 0;
    virtual void read(std::istream& in) {};
    virtual ~Entry() = default;

    virtual void p1(std::vector<const Entry*>& babies) const {};
    virtual void p2(std::vector<const Entry*>& bigboy, const int& threshold) const {};
};

class File: public Entry {
    int byte_count;
public:
    File(const int& n, const std::string& s): Entry{s}, byte_count{n} {}

    int size() const override {
        return byte_count;
    }
};

class Dir: public Entry {
    std::vector<std::unique_ptr<Entry>> subs;

public:
    Dir(const std::string& s): Entry{s}, subs{} {}

    int size() const override {
        int sum = 0;
        for (const auto& e: subs) {
            sum += e->size();
        }
        return sum;
    }

    void read(std::istream& in) override {
        std::string s;
        while (in >> s) {
            if (s == "ls") {
                while (in >> s) {
                    if (s == "$") break;
                    if (s == "dir") {
                        in >> s;
                        subs.emplace_back(std::make_unique<Dir>(s));
                    } else {
                        std::string tmp;
                        in >> tmp;
                        subs.emplace_back(std::make_unique<File>(stoi(s), tmp));
                    }
                }
            } else if (s == "cd") {
                in >> s;
                if (s == "..") break;
                for (auto& d: subs) {
                    if (d->getName() == s) {
                        in >> s;
                        d->read(in);
                    }
                }
            }
        }
    }

    void p1(std::vector<const Entry*>& babies) const override {
        if (size() <= 100000) {
            babies.emplace_back(this);
        }
        for (const auto& d: subs) {
            d->p1(babies);
        }
    }

    void p2(std::vector<const Entry*>& bigboy, const int& threshold) const override {
        if (size() > threshold) {
            bigboy.emplace_back(this);
        }
        for (const auto& d: subs) {
            d->p2(bigboy, threshold);
        }
    };
};

int main() {
    std::ifstream input{"day7.txt"};
    std::unique_ptr<Entry> filesystem = std::make_unique<Dir>("/");
    filesystem->read(input);

    std::vector<const Entry*> one;
    filesystem->p1(one);
    int part_one_ans = 0;
    for (const auto& d: one) {
        part_one_ans += d->size();
    }
    std::cout << "Part 1: " << part_one_ans << std::endl;

    const int tmp = 30000000 - (70000000 - filesystem->size());
    std::vector<const Entry*> two;
    filesystem->p2(two, tmp);
    std::sort(two.begin(), two.end());
    std::cout << "Part 2: " << two.back()->size() << std::endl;
}
