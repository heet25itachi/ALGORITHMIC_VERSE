package main

import "fmt"

type CacheEntry struct {
    key, value int
}

type FIFOCache struct {
    capacity int
    items    []CacheEntry
}

func NewFIFOCache(capacity int) *FIFOCache {
    return &FIFOCache{capacity: capacity, items: make([]CacheEntry, 0, capacity)}
}

func (c *FIFOCache) get(key int) int {
    for _, item := range c.items {
        if item.key == key {
            fmt.Print("Cache after get(", key, "): [")
            for i, item := range c.items {
                fmt.Print("(", item.key, ", ", item.value, ")")
                if i < len(c.items)-1 {
                    fmt.Print(", ")
                }
            }
            fmt.Println("]")
            return item.value
        }
    }
    return -1
}

func (c *FIFOCache) put(key, value int) {
    for i, item := range c.items {
        if item.key == key {
            c.items[i].value = value
            fmt.Print("Cache after put(", key, ", ", value, "): [")
            for i, item := range c.items {
                fmt.Print("(", item.key, ", ", item.value, ")")
                if i < len(c.items)-1 {
                    fmt.Print(", ")
                }
            }
            fmt.Println("]")
            return
        }
    }
    if len(c.items) == c.capacity {
        c.items = c.items[1:]
    }
    c.items = append(c.items, CacheEntry{key, value})
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
    cache := NewFIFOCache(3)
    cache.put(1, 10)
    cache.put(2, 20)
    cache.put(3, 30)
    cache.put(4, 40)
    fmt.Printf("Get(2) = %d\n", cache.get(2))
    cache.put(5, 50)
}
