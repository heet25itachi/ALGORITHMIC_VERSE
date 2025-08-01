#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

class LRUCache {
private:
    int capacity;
    vector<pair<int, int>> items;
    unordered_map<int, int> key_to_index;

public:
    LRUCache(int cap) : capacity(cap) {}

    int get(int key) {
        if (key_to_index.find(key) == key_to_index.end()) return -1;
        int idx = key_to_index[key];
        int value = items[idx].second;
        auto item = items[idx];
        items.erase(items.begin() + idx);
        items.push_back(item);
        key_to_index[key] = items.size() - 1;
        cout << "Cache after get(" << key << "): [";
        for (int i = 0; i < items.size(); i++) {
            cout << "(" << items[i].first << ", " << items[i].second << ")" << (i < items.size() - 1 ? ", " : "");
        }
        cout << "]" << endl;
        return value;
    }

    void put(int key, int value) {
        if (key_to_index.find(key) != key_to_index.end()) {
            int idx = key_to_index[key];
            items.erase(items.begin() + idx);
        } else if (items.size() == capacity) {
            key_to_index.erase(items[0].first);
            items.erase(items.begin());
        }
        items.push_back({key, value});
        key_to_index[key] = items.size() - 1;
        cout << "Cache after put(" << key << ", " << value << "): [";
        for (int i = 0; i < items.size(); i++) {
            cout << "(" << items[i].first << ", " << items[i].second << ")" << (i < items.size() - 1 ? ", " : "");
        }
        cout << "]" << endl;
    }
};

int main() {
    LRUCache cache(3);
    cache.put(1, 10);
    cache.put(2, 20);
    cache.put(3, 30);
    cache.put(4, 40);
    cout << "Get(2) = " << cache.get(2) << endl;
    cache.put(5, 50);
    return 0;
}
