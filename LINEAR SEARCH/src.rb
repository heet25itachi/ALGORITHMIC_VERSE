def linear_search(arr, target)
  arr.each_with_index do |val, i|
    return i if val == target
  end
  -1
end

arr = [3, 7, 1, 9, 4]
target = 9
result = linear_search(arr, target)
puts "Target #{target} found at index: #{result}"
