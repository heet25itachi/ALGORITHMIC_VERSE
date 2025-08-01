#include <iostream>
#include <vector>
using namespace std;

class FIFOCache {
private:
    int capacity;
    vector<pair<int, int>> items;

public:
    FIFOCache(int cap) : capacity(cap) {}

    int get(int key) {
        for (int i = 0; i < items.size(); i++) {
            if (items[i].first == key) {
                cout << "Cache after get(" << key << "): [";
                for (int j = 0; j < items.size(); j++) {
                    cout << "(" << items[j].first << ", " << items[j].second << ")" << (j < items.size() - 1 ? ", " : "");
                }
                cout << "]" << endl;
                return items[i].second;
            }
        }
        return -1;
    }

    void put(int key, int value) {
        for (int i = 0; i < items.size(); i++) {
            if (items[i].first == key) {
                items[i].second = value;
                cout << "Cache after put(" << key << ", " << value << "): [";
                for (int j = 0; j < items.size(); j++) {
                    cout << "(" << items[j].first << ", " << items[j].second << ")" << (j < items.size() - 1 ? ", " : "");
                }
                cout << "]" << endl;
                return;
            }
        }
        if (items.size() == capacity) {
            items.erase(items.begin());
        }
        items.push_back({key, value});
        cout << "Cache after put(" << key << ", " << value << "): [";
        for (int i = 0; i < items.size(); i++) {
            cout << "(" << items[i].first << ", " << items[i].second << ")" << (i < items.size() - 1 ? ", " : "");
        }
        cout << "]" << endl;
    }
};

int main() {
    FIFOCache cache(3);
    cache.put(1, 10);
    cache.put(2, 20);
    cache.put(3, 30);
    cache.put(4, 40);
    cout << "Get(2) = " << cache.get(2) << endl;
    cache.put(5, 50);
    return 0;
}
