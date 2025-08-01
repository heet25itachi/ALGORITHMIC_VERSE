#include <stdio.h>
#include <stdlib.h>
#define CAPACITY 3

typedef struct {
    int key, value;
} CacheEntry;

typedef struct {
    CacheEntry items[CAPACITY];
    int size;
} FIFOCache;

void init_cache(FIFOCache *cache) {
    cache->size = 0;
}

int get_index(FIFOCache *cache, int key) {
    for (int i = 0; i < cache->size; i++) {
        if (cache->items[i].key == key) return i;
    }
    return -1;
}

int get(FIFOCache *cache, int key) {
    int idx = get_index(cache, key);
    if (idx == -1) return -1;
    printf("Cache after get(%d): [", key);
    for (int i = 0; i < cache->size; i++) {
        printf("(%d, %d)%s", cache->items[i].key, cache->items[i].value, i < cache->size - 1 ? ", " : "");
    }
    printf("]\n");
    return cache->items[idx].value;
}

void put(FIFOCache *cache, int key, int value) {
    int idx = get_index(cache, key);
    if (idx != -1) {
        cache->items[idx].value = value;
    } else {
        if (cache->size == CAPACITY) {
            for (int i = 0; i < cache->size - 1; i++) {
                cache->items[i] = cache->items[i + 1];
            }
            cache->size--;
        }
        cache->items[cache->size].key = key;
        cache->items[cache->size].value = value;
        cache->size++;
    }
    printf("Cache after put(%d, %d): [", key, value);
    for (int i = 0; i < cache->size; i++) {
        printf("(%d, %d)%s", cache->items[i].key, cache->items[i].value, i < cache->size - 1 ? ", " : "");
    }
    printf("]\n");
}

int main() {
    FIFOCache cache;
    init_cache(&cache);
    put(&cache, 1, 10);
    put(&cache, 2, 20);
    put(&cache, 3, 30);
    put(&cache, 4, 40);
    printf("Get(2) = %d\n", get(&cache, 2));
    put(&cache, 5, 50);
    return 0;
}
