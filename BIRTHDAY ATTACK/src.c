#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#define R 65536
#define TABLE_SIZE 65536

typedef struct {
    long long input;
    int hash;
} Entry;

void birthday_attack() {
    Entry* table = calloc(TABLE_SIZE, sizeof(Entry));
    srand(time(NULL));
    int trials = 0;
    while (1) {
        long long x = ((long long)rand() << 32) | rand(); // 64-bit random
        int h = x % R;
        trials++;
        if (table[h].input != 0 && table[h].input != x) {
            printf("Collision found after %d trials: x_i = %lld, x_j = %lld, hash = %d\n",
                   trials, table[h].input, x, h);
            break;
        }
        table[h].input = x;
    }
    free(table);
}

int main() {
    birthday_attack();
    return 0;
}
