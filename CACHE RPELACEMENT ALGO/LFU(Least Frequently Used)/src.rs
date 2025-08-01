package main

import "fmt"

type CacheEntry struct {
    key, value, freq int
}

type LFUCache struct {
    capacity int
    items    []CacheEntry
    keyMap   map[int]int
}

func NewLFUCache(capacity int) *LFUCache {
    return &LFUCache{
        capacity: capacity,
        items:    make([]CacheEntry, 0, capacity),
        keyMap:   make(map[int]int),
    }
}

func (c *LFUCache) get(key int) int {
    if idx, exists := c.keyMap[key]; exists {
        c.items[idx].freq++
        fmt.Print("Cache after get(", key, "): [")
        for i, item := range c.items {
            fmt.Print("(", item.key, ", ", item.value, ")")
            if i < len(c.items)-1 {
                fmt.Print(", ")
            }
        }
        fmt.Println("]")
        return c.items[idx].value
    }
    return -1
}

func (c *LFUCache) put(key, value int) {
    if idx, exists := c.keyMap[key]; exists {
        c.items[idx].value = value
        c.items[idx].freq++
    } else {
        if len(c.items) == c.capacity {
            min_freq := c.items[0].freq
            min_idx := 0
            for i, item := range c.items {
                if item.freq < min_freq || (item.freq == min_freq && i < min_idx) {
                    min_freq = item.freq
                    min_idx = i
                }
            }
            delete(c.keyMap, c.items[min_idx].key)
            c.items = append(c.items[:min_idx], c.items[min_idx+1:]...)
        }
        c.items = append(c.items, CacheEntry{key, value, 1})
        c.keyMap[key] = len(c.items) - 1
    }
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
    cache := NewLFUCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    fmt.Printf("Get(2) = %d\n", cache.get(2))
    cache.put(5, 50)
}
