mutable struct FIFOCache
    capacity::Int
    cache::Dict{Int,Int}
    order::Vector{Int}
    FIFOCache(capacity) = new(capacity, Dict{Int,Int}(), Int[])
end

function get(cache::FIFOCache, key::Int)
    if !haskey(cache.cache, key)
        return -1
    end
    print("Cache after get($key): [")
    for (i, k) in enumerate(cache.order)
        print("($k, $(cache.cache[k]))" * (i < length(cache.order) ? ", " : ""))
    end
    println("]")
    return cache.cache[key]
end

function put(cache::FIFOCache, key::Int, value::Int)
    if haskey(cache.cache, key)
        cache.cache[key] = value
    else
        if length(cache.cache) == cache.capacity
            delete!(cache.cache, popfirst!(cache.order))
        end
        cache.cache[key] = value
        push!(cache.order, key)
    end
    print("Cache after put($key, $value): [")
    for (i, k) in enumerate(cache.order)
        print("($k, $(cache.cache[k]))" * (i < length(cache.order) ? ", " : ""))
    end
    println("]")
end

cache = FIFOCache(3)
put(cache, 1, 10)
put(cache, 2, 20)
put(cache, 3, 30)
put(cache, 4, 40)
println("Get(2) = $(get(cache, 2))")
put(cache, 5, 50)
