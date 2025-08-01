package main

import "fmt"

type CacheEntry struct {
    key, value int
}

type LRUCache struct {
    capacity int
    items    []CacheEntry
    keyMap   map[int]int
}

func NewLRUCache(capacity int) *LRUCache {
    return &LRUCache{
        capacity: capacity,
        items:    make([]CacheEntry, 0, capacity),
        keyMap:   make(map[int]int),
    }
}

func (c *LRUCache) get(key int) int {
    if idx, exists := c.keyMap[key]; exists {
        value := c.items[idx].value
        item := c.items[idx]
        c.items = append(c.items[:idx], c.items[idx+1:]...)
        c.items = append(c.items, item)
        c.keyMap[key] = len(c.items) - 1
        fmt.Print("Cache after get(", key, "): [")
        for i, item := range c.items {
            fmt.Print("(", item.key, ", ", item.value, ")")
            if i < len(c.items)-1 {
                fmt.Print(", ")
            }
        }
        fmt.Println("]")
        return value
    }
    return -1
}

func (c *LRUCache) put(key, value int) {
    if idx, exists := c.keyMap[key]; exists {
        c.items = append(c.items[:idx], c.items[idx+1:]...)
        delete(c.keyMap, key)
    } else if len(c.items) == c.capacity {
        delete(c.keyMap, c.items[0].key)
        c.items = c.items[1:]
    }
    c.items = append(c.items, CacheEntry{key, value})
    c.keyMap[key] = len(c.items) - 1
    fmt.Print("Cache after put(", key, ", ", value, "): [")
    for i, item := range c.items {
        fmt.Print("(", item.key, ", ", item.value, ")")
        if i < len(c.items)-1 {
            fmt.Print(", ")
        }
    }
    fmt.Println("]")
}

func main() {
    cache := NewLRUCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    fmt.Printf("Get(2) = %d\n", cache.get(2))
    cache.put(5, 50)
}
