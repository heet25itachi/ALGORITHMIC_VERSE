class MRUCache:
    def __init__(self, capacity):
        self.capacity = capacity
        self.cache = {}
        self.order = []

    def get(self, key):
        if key not in self.cache:
            return -1
        self.order.remove(key)
        self.order.insert(0, key)
        print(f"Cache after get({key}): {[(k, self.cache[k]) for k in self.order]}")
        return self.cache[key]

    def put(self, key, value):
        if key in self.cache:
            self.order.remove(key)
        elif len(self.cache) == self.capacity:
            oldest = self.order.pop(0)
            del self.cache[oldest]
        self.cache[key] = value
        self.order.insert(0, key)
        print(f"Cache after put({key}, {value}): {[(k, self.cache[k]) for k in self.order]}")

if __name__ == "__main__":
    cache = MRUCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    print(f"Get(2) = {cache.get(2)}")
    cache.put(5, 50)
