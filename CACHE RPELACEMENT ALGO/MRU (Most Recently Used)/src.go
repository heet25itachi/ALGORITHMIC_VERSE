package main

import "fmt"

type CacheEntry struct {
    key, value int
}

type MRUCache struct {
    capacity int
    items    []CacheEntry
    keyMap   map[int]int
}

func NewMRUCache(capacity int) *MRUCache {
    return &MRUCache{
        capacity: capacity,
        items:    make([]CacheEntry, 0, capacity),
        keyMap:   make(map[int]int),
    }
}

func (c *MRUCache) get(key int) int {
    if idx, exists := c.keyMap[key]; exists {
        value := c.items[idx].value
        item := c.items[idx]
        c.items = append(c.items[:idx], c.items[idx+1:]...)
        c.items = append([]CacheEntry{item}, c.items...)
        c.keyMap[key] = 0
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

func (c *MRUCache) put(key, value int) {
    if idx, exists := c.keyMap[key]; exists {
        c.items = append(c.items[:idx], c.items[idx+1:]...)
        delete(c.keyMap, key)
    } else if len(c.items) == c.capacity {
        delete(c.keyMap, c.items[0].key)
        c.items = c.items[1:]
    }
    c.items = append([]CacheEntry{{key, value}}, c.items...)
    c.keyMap[key] = 0
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
    cache := NewMRUCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    fmt.Printf("Get(2) = %d\n", cache.get(2))
    cache.put(5, 50)
}
