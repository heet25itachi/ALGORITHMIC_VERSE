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

function k_merger(inputs::Vector{Vector{Int}}, input_sizes::Vector{Int}, k::Int, buffer::Vector{Int}, buffer_size::Int, k3::Int)
    if k == 1
        size = min(input_sizes[1], k3)
        output = inputs[1][1:size]
        inputs[1] = inputs[1][size + 1:end]
        input_sizes[1] -= size
        return output
    end

    sqrt_k = floor(Int, sqrt(k))
    input_mergers = [inputs[i * sqrt_k + 1] for i in 0:sqrt_k-1]
    input_merger_sizes = [input_sizes[i * sqrt_k + 1] for i in 0:sqrt_k-1]
    sub_buffers = [zeros(Int, buffer_size) for _ in 1:sqrt_k]
    sub_buffer_sizes = zeros(Int, sqrt_k)

    k32 = floor(Int, k^1.5)
    for i in 1:sqrt_k
        if sub_buffer_sizes[i] < k32
            temp_output = k_merger(input_mergers, input_merger_sizes, sqrt_k, sub_buffers[i], buffer_size, k32)
            sub_buffers[i][1:length(temp_output)] = temp_output
            sub_buffer_sizes[i] = length(temp_output)
        end
    end

    output_merger_inputs = sub_buffers
    output_merger_sizes = sub_buffer_sizes
    output = k_merger(output_merger_inputs, output_merger_sizes, sqrt_k, buffer, buffer_size, k3)

    for i in 1:sqrt_k
        sub_buffers[i] = sub_buffers[i][length(output) + 1:end]
        sub_buffer_sizes[i] -= length(output)
    end

    output
end

function funnelsort!(arr::Vector{Int}, z::Int, l::Int)
    n = length(arr)
    if n <= z
        quicksort!(arr, 1, n)
        return
    end

    k = ceil(Int, n^(1/3))
    sub_size = ceil(Int, n / k)
    subarrays = Vector{Vector{Int}}(undef, k)
    subarray_sizes = Vector{Int}(undef, k)

    println("Sorted subarrays:")
    for i in 1:k
        size = i == k ? n - (i-1) * sub_size : sub_size
        subarrays[i] = arr[(i-1)*sub_size + 1:(i-1)*sub_size + size]
        subarray_sizes[i] = size
        quicksort!(subarrays[i], 1, size)
        println("Subarray ", i-1, ": ", join(subarrays[i], " "))
    end

    buffer_size = 2 * floor(Int, k^1.5)
    buffer = zeros(Int, buffer_size)
    output = k_merger(subarrays, subarray_sizes, k, buffer, buffer_size, n)

    arr[1:n] = output
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
z = 8
l = 2

println("Initial array: ", join(arr, " "))

funnelsort!(arr, z, l)

println("Sorted array: ", join(arr, " "))
