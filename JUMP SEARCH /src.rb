def jump_search(arr, target)
  size = arr.length
  step = Math.sqrt(size).to_i
  prev = 0
  while arr[[step, size].min - 1] < target
    prev = step
    step += Math.sqrt(size).to_i
    return -1 if prev >= size
  end
  while prev < size && arr[prev] < target
    prev += 1
  end
  prev < size && arr[prev] == target ? prev : -1
end

arr = [1, 3, 4, 7, 9]
target = 9
result = jump_search(arr, target)
puts "Target #{target} found at index: #{result}"
