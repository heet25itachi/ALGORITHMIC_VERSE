function quicksort!(arr::Vector{Int}, left::Int, right::Int)
    if left >= right
        return
    end
    pivot = arr[right + 1]
    i = left + 1
    j = right + 1
    while i < j
        while i < j && arr[i] <= pivot
            i += 1
        end
        while i < j && arr[j] > pivot
            j -= 1
        end
        if i < j
            arr[i], arr[j] = arr[j], arr[i]
        end
    end
    arr[right + 1], arr[i] = arr[i], arr[right + 1]
    quicksort!(arr, left, i - 1)
    quicksort!(arr, i + 1, right)
end

function pem_select(arr::Vector{Int}, n::Int, k::Int)
    if n <= 5
        temp = sort(arr[1:n])
        return temp[k]
    end
    arr[k] # Simplified: use direct access for small arrays
end

function pem_multipartition(arr::Vector{Int}, n::Int, pivots::Vector{Int}, d_sqrt::Int, p::Int)
    counts = zeros(Int, p * d_sqrt)
    for i in 1:p
        start = (i-1) * (n ÷ p) + 1
        size = i == p ? n - (i-1) * (n ÷ p) : n ÷ p
        for j in 1:size
            elem = arr[start + j - 1]
            bucket = 1
            while bucket < d_sqrt && elem > pivots[bucket]
                bucket += 1
            end
            counts[(i-1) * d_sqrt + bucket] += 1
        end
    end

    prefix_sums = zeros(Int, d_sqrt)
    for j in 1:d_sqrt
        for i in 1:p
            prefix_sums[j] += counts[(i-1) * d_sqrt + j]
        end
    end

    buckets = [Int[] for _ in 1:d_sqrt]
    for j in 1:d_sqrt
        buckets[j] = zeros(Int, prefix_sums[j])
    end

    offsets = zeros(Int, d_sqrt)
    for i in 1:p
        start = (i-1) * (n ÷ p) + 1
        size = i == p ? n - (i-1) * (n ÷ p) : n ÷ p
        for j in 1:size
            elem = arr[start + j - 1]
            bucket = 1
            while bucket < d_sqrt && elem > pivots[bucket]
                bucket += 1
            end
            offsets[bucket] += 1
            buckets[bucket][offsets[bucket]] = elem
        end
    end

    return buckets, prefix_sums
end

function pem_dist_sort!(arr::Vector{Int}, p::Int, m::Int, b::Int, d::Int)
    n = length(arr)
    if n <= m
        quicksort!(arr, 1, n)
        return
    end

    d_sqrt = floor(Int, sqrt(d))
    segment_size = n ÷ p

    println("Segments:")
    for i in 1:p
        size = i == p ? n - (i-1) * segment_size : segment_size
        println("Segment ", i-1, ": ", join(arr[(i-1)*segment_size + 1:(i-1)*segment_size + size], " "))
    end

    pivots = [pem_select(arr, n, (j * n) ÷ d_sqrt) for j in 1:d_sqrt-1]
    println("Pivots: ", join(pivots, " "))

    buckets, bucket_sizes = pem_multipartition(arr, n, pivots, d_sqrt, p)

    println("Buckets:")
    for j in 1:d_sqrt
        println("Bucket ", j-1, ": ", join(buckets[j], " "))
    end

    output = zeros(Int, n)
    output_pos = 1
    for j in 1:d_sqrt
        processors = ceil(Int, bucket_sizes[j] / (n / p))
        pem_dist_sort!(buckets[j], processors, m, b, d)
        output[output_pos:output_pos + bucket_sizes[j] - 1] = buckets[j]
        output_pos += bucket_sizes[j]
    end

    arr[1:n] = output
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
p = 4
m = 8
b = 2
d = 4

println("Initial array: ", join(arr, " "))

pem_dist_sort!(arr, p, m, b, d)

println("Sorted array: ", join(arr, " "))
