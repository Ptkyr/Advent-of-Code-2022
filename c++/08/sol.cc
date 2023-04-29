#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

class Trees {
    std::vector<std::vector<int>> grid;

    bool trivial(const int& x, const int& y) const {
        const int x_max = grid.size();
        const int y_max = grid[0].size();
        if (x == 0 || x == x_max - 1) return true;
        if (y == 0 || y == y_max - 1) return true;
        return false;
    }

    bool leftVis(const int& x, const int& y) const {
        if (trivial(x, y)) return true;
        for (int i = 0; i < y; ++i) {
            if (grid[x][i] >= grid[x][y]) return false;
        }
        return true;
    }

    bool rightVis(const int& x, const int& y) const {
        if (trivial(x, y)) return true;
        const int y_max = grid[0].size();
        for (int i = y + 1; i < y_max; ++i) {
            if (grid[x][i] >= grid[x][y]) return false;
        }
        return true;
    }

    bool topVis(const int& x, const int& y) const {
        if (trivial(x, y)) return true;
        for (int i = 0; i < x; ++i) {
            if (grid[i][y] >= grid[x][y]) return false;
        }
        return true;
    }

    bool botVis(const int& x, const int& y) const {
        if (trivial(x, y)) return true;
        const int x_max = grid.size();
        for (int i = x + 1; i < x_max; ++i) {
            if (grid[i][y] >= grid[x][y]) return false;
        }
        return true;
    }

    bool visible(const int& x, const int& y) const {
        return (leftVis(x, y) || rightVis(x, y) ||
                topVis(x, y) || botVis(x, y));
    }

    int leftSight(const int& x, const int& y) const {
        int see = 0;
        for (int i = y - 1; i >= 0; --i) {
            ++see;
            if (grid[x][i] >= grid[x][y]) break;
        }
        return see;
    }

    int rightSight(const int& x, const int& y) const {
        int see = 0;
        const int y_max = grid[0].size();
        for (int i = y + 1; i < y_max; ++i) {
            ++see;
            if (grid[x][i] >= grid[x][y]) break;
        }
        return see;
    }

    int topSight(const int& x, const int& y) const {
        int see = 0;
        for (int i = x - 1; i >= 0; --i) {
            ++see;
            if (grid[i][y] >= grid[x][y]) break;
        }
        return see;
    }

    int botSight(const int& x, const int& y) const {
        int see = 0;
        const int x_max = grid.size();
        for (int i = x + 1; i < x_max; ++i) {
            ++see;
            if (grid[i][y] >= grid[x][y]) break;
        }
        return see;
    }

    int scenic(const int& x, const int& y) const {
        return leftSight(x, y) * rightSight(x, y) *
               topSight(x, y) * botSight(x, y);
    }

public:
    int p1() const {
        const int x_max = grid.size();
        const int y_max = grid[0].size();
        int count = 0;
        for (int i = 0; i < x_max; ++i) {
            for (int j = 0; j < y_max; ++j) {
                if (visible(i, j)) ++count;
            }
        }
        return count;
    }

    int p2() const {
        const int x_max = grid.size();
        const int y_max = grid[0].size();
        int max = 0;
        for (int i = 0; i < x_max; ++i) {
            for (int j = 0; j < y_max; ++j) {
                const int score = scenic(i, j);
                if (score > max) max = score;
            }
        }
        return max;
    }

    friend std::istream& operator>>(std::istream& in, Trees& t) {
        std::string s;
        int i = 0;
        while (getline(in, s)) {
            t.grid.emplace_back();
            std::istringstream iss{s};
            char n;
            while (iss >> n) {
                t.grid[i].emplace_back(n - '0');
            }
            ++i;
        }
        return in;
    }

    friend std::ostream& operator<<(std::ostream& out, const Trees& t) {
        const int x_max = t.grid.size();
        const int y_max = t.grid[0].size();
        for (int i = 0; i < x_max; ++i) {
            for (int j = 0; j < y_max; ++j) {
                out << t.scenic(i, j);
            }
            out << std::endl;
        }
        return out;
    }
};

int main() {
    std::ifstream input{"day8.txt"};
    Trees t;
    input >> t;
    std::cout << "Part 1: " << t.p1() << std::endl;
    std::cout << "Part 2: " << t.p2() << std::endl;
}
