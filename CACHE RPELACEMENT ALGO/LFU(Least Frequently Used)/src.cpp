#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

class LFUCache {
private:
    int capacity;
    vector<pair<int, pair<int, int>>> items; // key, (value, freq)
    unordered_map<int, int> key_to_index;

public:
    LFUCache(int cap) : capacity(cap) {}

    int get(int key) {
        if (key_to_index.find(key) == key_to_index.end()) return -1;
        int idx = key_to_index[key];
        int value = items[idx].second.first;
        items[idx].second.second++;
        key_to_index[key] = idx;
        cout << "Cache after get(" << key << "): [";
        for (int i = 0; i < items.size(); i++) {
            cout << "(" << items[i].first << ", " << items[i].second.first << ")" << (i < items.size() - 1 ? ", " : "");
        }
        cout << "]" << endl;
        return value;
    }

    void put(int key, int value) {
        if (key_to_index.find(key) != key_to_index.end()) {
            int idx = key_to_index[key];
            items[idx].second.first = value;
            items[idx].second.second++;
        } else {
            if (items.size() == capacity) {
                int min_freq = items[0].second.second, min_idx = 0;
                for (int i = 1; i < items.size(); i++) {
                    if (items[i].second.second < min_freq || (items[i].second.second == min_freq && i < min_idx)) {
                        min_freq = items[i].second.second;
                        min_idx = i;
                    }
                }
                key_to_index.erase(items[min_idx].first);
                items.erase(items.begin() + min_idx);
            }
            items.push_back({key, {value, 1}});
            key_to_index[key] = items.size() - 1;
        }
        cout << "Cache after put(" << key << ", " << value << "): [";
        for (int i = 0; i < items.size(); i++) {
            cout << "(" << items[i].first << ", " << items[i].second.first << ")" << (i < items.size() - 1 ? ", " : "");
        }
        cout << "]" << endl;
    }
};

int main() {
    LFUCache cache(3);
    cache.put(1, 10);
    cache.put(2, 20);
    cache.put(3, 30);
    cache.put(4, 40);
    cout << "Get(2) = " << cache.get(2) << endl;
    cache.put(5, 50);
    return 0;
}
