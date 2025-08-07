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

function merge_runs(run1::Vector{Int}, run2::Vector{Int}, B::Int)
    output = Int[]
    i = j = 1
    while i <= length(run1) && j <= length(run2)
        for _ in 1:B
            if i <= length(run1) && (j > length(run2) || run1[i] <= run2[j])
                push!(output, run1[i])
                i += 1
            elseif j <= length(run2)
                push!(output, run2[j])
                j += 1
            end
        end
    end
    append!(output, run1[i:end])
    append!(output, run2[j:end])
    output
end

function external_merge_sort(arr::Vector{Int}, M::Int, B::Int)
    n = length(arr)
    if n <= M
        quicksort(arr, 1, n)
        return
    end

    # Step 1: Divide and sort runs
    runs = Vector{Int}[]
    for i in 1:M:n
        run = arr[i:min(i+M-1, n)]
        quicksort(run, 1, length(run))
        push!(runs, run)
    end

    # Print sorted runs
    println("Sorted runs:")
    for (i, run) in enumerate(runs)
        println("Run $i: ", run)
    end

    # Step 2: Merge runs (2-way merge for M/B = 2)
    output = merge_runs(runs[1], runs[2], B)
    arr[1:n] = output
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
M = 8
B = 4
println("Initial array: ", arr)
external_merge_sort(arr, M, B)
println("Sorted array: ", arr)
