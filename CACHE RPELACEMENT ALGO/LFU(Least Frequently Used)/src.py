class LFUCache:
    def __init__(self, capacity):
        self.capacity = capacity
        self.cache = {}
        self.freq = {}
        self.order = []

    def get(self, key):
        if key not in self.cache:
            return -1
        self.freq[key] += 1
        print(f"Cache after get({key}): {[(k, self.cache[k]) for k in self.order]}")
        return self.cache[key]

    def put(self, key, value):
        if key in self.cache:
            self.cache[key] = value
            self.freq[key] += 1
        else:
            if len(self.cache) == self.capacity:
                min_freq = min(self.freq.values())
                min_key = next(k for k in self.order if self.freq[k] == min_freq)
                del self.cache[min_key]
                del self.freq[min_key]
                self.order.remove(min_key)
            self.cache[key] = value
            self.freq[key] = 1
            self.order.append(key)
        print(f"Cache after put({key}, {value}): {[(k, self.cache[k]) for k in self.order]}")

if __name__ == "__main__":
    cache = LFUCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    print(f"Get(2) = {cache.get(2)}")
    cache.put(5, 50)
