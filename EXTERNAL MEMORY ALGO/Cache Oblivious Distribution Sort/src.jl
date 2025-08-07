mutable struct Bucket
    elements::Vector{Int}
    pivot::Int
    Bucket() = new(Vector{Int}(), 10^9)
end

function approximate_median(arr::Vector{Int})::Int
    isempty(arr) && return 0
    mid = div(length(arr), 2) + 1
    values = [arr[1], arr[mid], arr[end]]
    return sort(values)[2]
end

function quicksort(arr::Vector{Int}, left::Int, right::Int)
    if left >= right
        return
    end
    pivot = arr[right]
    i, j = left, right
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
    arr[i], arr[right] = arr[right], arr[i]
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)
end

function copy_elems(arr::Vector{Int}, next::Vector{Int}, bnum::Vector{Int}, buckets::Vector{Bucket}, subarray_size::Int, bucket_idx::Int, sqrt_n::Int)
    while next[bucket_idx + 1] <= subarray_size
        if bnum[bucket_idx + 1] >= length(buckets)
            push!(buckets, Bucket())
        end
        if arr[next[bucket_idx + 1]] <= buckets[bnum[bucket_idx + 1] + 1].pivot
            if length(buckets[bnum[bucket_idx + 1] + 1].elements) >= 2 * sqrt_n
                median = approximate_median(buckets[bnum[bucket_idx + 1] + 1].elements)
                push!(buckets, Bucket())
                buckets[end].pivot = buckets[bnum[bucket_idx + 1] + 1].pivot
                buckets[bnum[bucket_idx + 1] + 1].pivot = median
                new_elements = filter(x -> x <= median, buckets[bnum[bucket_idx + 1] + 1].elements)
                buckets[end].elements = filter(x -> x > median, buckets[bnum[bucket_idx + 1] + 1].elements)
                buckets[bnum[bucket_idx + 1] + 1].elements = new_elements
                for i in eachindex(bnum)
                    if bnum[i] > bnum[bucket_idx + 1]
                        bnum[i] += 1
                    end
                end
            end
            push!(buckets[bnum[bucket_idx + 1] + 1].elements, arr[next[bucket_idx + 1]])
            next[bucket_idx + 1] += 1
        else
            bnum[bucket_idx + 1] += 1
        end
    end
end

function distribute(arr::Vector{Int}, next::Vector{Int}, bnum::Vector{Int}, i::Int, j::Int, m::Int, buckets::Vector{Bucket}, sqrt_n::Int)
    if m == 1
        copy_elems(arr[i * sqrt_n + 1 : (i + 1) * sqrt_n], next, bnum, buckets, sqrt_n, i, sqrt_n)
    else
        distribute(arr, next, bnum, i, j, div(m, 2), buckets, sqrt_n)
        distribute(arr, next, bnum, i + div(m, 2), j, div(m, 2), buckets, sqrt_n)
        distribute(arr, next, bnum, i, j + div(m, 2), div(m, 2), buckets, sqrt_n)
        distribute(arr, next, bnum, i + div(m, 2), j + div(m, 2), div(m, 2), buckets, sqrt_n)
    end
end

function cache_oblivious_sort(arr::Vector{Int})
    n = length(arr)
    n <= 1 && return
    sqrt_n = Int(sqrt(n))
    sqrt_n * sqrt_n != n && return

    # Step 1: Partition and sort subarrays
    for i in 0:sqrt_n-1
        quicksort(arr, i * sqrt_n + 1, (i + 1) * sqrt_n)
    end

    # Step 2: Distribute
    next = zeros(Int, sqrt_n)
    bnum = zeros(Int, sqrt_n)
    buckets = [Bucket()]
    distribute(arr, next, bnum, 0, 0, sqrt_n, buckets, sqrt_n)

    # Step 3: Sort buckets
    for bucket in buckets
        sort!(bucket.elements)
    end

    # Step 4: Concatenate
    k = 1
    for bucket in buckets
        for x in bucket.elements
            arr[k] = x
            k += 1
        end
    end
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
println("Initial array: ", arr)
cache_oblivious_sort(arr)
println("Sorted array: ", arr)
