function jump_search(arr, target)
    size = length(arr)
    step = floor(Int, sqrt(size))
    prev = 1
    while arr[min(step, size)] < target
        prev = step
        step += floor(Int, sqrt(size))
        if prev > size
            return -1
        end
    end
    while prev <= size && arr[prev] < target
        prev += 1
    end
    if prev <= size && arr[prev] == target
        return prev
    end
    return -1
end

arr = [1, 3, 4, 7, 9]
target = 9
result = jump_search(arr, target)
println("Target $target found at index: $result")
