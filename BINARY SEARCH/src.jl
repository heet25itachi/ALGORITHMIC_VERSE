function binary_search(arr, target)
    left, right = 1, length(arr)
    while left <= right
        mid = (left + right) ÷ 2
        if arr[mid] == target
            return mid
        elseif arr[mid] < target
            left = mid + 1
        else
            right = mid - 1
        end
    end
    return -1
end

arr = [1, 3, 4, 7, 9]
target = 9
result = binary_search(arr, target)
println("Target $target found at index: $result")
