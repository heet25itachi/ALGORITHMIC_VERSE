function linear_search(arr, target)
    for i in 1:length(arr)
        if arr[i] == target
            return i
        end
    end
    return -1
end

arr = [3, 7, 1, 9, 4]
target = 9
result = linear_search(arr, target)
println("Target $target found at index: $result")
