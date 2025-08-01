def binary_search(arr, target)
  left = 0
  right = arr.length - 1
  while left <= right
    mid = (left + right) / 2
    if arr[mid] == target
      return mid
    elsif arr[mid] < target
      left = mid + 1
    else
      right = mid - 1
    end
  end
  -1
end

arr = [1, 3, 4, 7, 9]
target = 9
result = binary_search(arr, target)
puts "Target #{target} found at index: #{result}"
