mutable struct LFUCache
    capacity::Int
    cache::Dict{Int,Int}
    freq::Dict{Int,Int}
    order::Vector{Int}
    LFUCache(capacity) = new(capacity, Dict{Int,Int}(), Dict{Int,Int}(), Int[])
end

function get(cache::LFUCache, key::Int)
    if !haskey(cache.cache, key)
        return -1
    end
    cache.freq[key] += 1
    print("Cache after get($key): [")
    for (i, k) in enumerate(cache.order)
        print("($k, $(cache.cache[k]))" * (i < length(cache.order) ? ", " : ""))
    end
    println("]")
    return cache.cache[key]
end

function put(cache::LFUCache, key::Int, value::Int)
    if haskey(cache.cache, key)
        cache.cache[key] = value
        cache.freq[key] += 1
    else
        if length(cache.cache) == cache.capacity
            min_freq = minimum(values(cache.freq))
            min_key = first(k for k in cache.order if cache.freq[k] == min_freq)
            delete!(cache.cache, min_key)
            delete!(cache.freq, min_key)
            filter!(k -> k != min_key, cache.order)
        end
        cache.cache[key] = value
        cache.freq[key] = 1
        push!(cache.order, key)
    end
    print("Cache after put($key, $value): [")
    for (i, k) in enumerate(cache.order)
        print("($k, $(cache.cache[k]))" * (i < length(cache.order) ? ", " : ""))
    end
    println("]")
end

cache = LFUCache(3)
put(cache, 1, 10)
put(cache, 2, 20)
put(cache, 3, 30)
put(cache, 4, 40)
println("Get(2) = $(get(cache, 2))")
put(cache, 5, 50)
