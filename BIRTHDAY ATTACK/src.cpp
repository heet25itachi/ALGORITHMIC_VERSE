#include <iostream>
#include <unordered_map>
#include <random>

const int R = 65536;

void birthday_attack() {
    std::unordered_map<int, long long> table;
    std::mt19937_64 rng(std::random_device{}());
    int trials = 0;
    while (true) {
        long long x = rng();
        int h = x % R;
        trials++;
        if (table.find(h) != table.end() && table[h] != x) {
            std::cout << "Collision found after " << trials
                      << " trials: x_i = " << table[h] << ", x_j = " << x
                      << ", hash = " << h << std::endl;
            break;
        }
        table[h] = x;
    }
}

int main() {
    birthday_attack();
    return 0;
}
