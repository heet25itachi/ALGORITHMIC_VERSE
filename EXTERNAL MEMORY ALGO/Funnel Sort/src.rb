def quicksort(arr, left, right)
  return if left >= right
  pivot = arr[right]
  i = left
  j = right
  while i < j
    i += 1 while i < j && arr[i] <= pivot
    j -= 1 while i < j && arr[j] > pivot
    arr[i], arr[j] = arr[j], arr[i] if i < j
  end
  arr[right], arr[i] = arr[i], arr[right]
  quicksort(arr, left, i - 1)
  quicksort(arr, i + 1, right)
end

def k_merger(inputs, input_sizes, k, buffer, buffer_size, k3)
  if k == 1
    size = [input_sizes[0], k3].min
    output = inputs[0][0...size]
    inputs[0] = inputs[0][size..-1] || []
    input_sizes[0] -= size
    return output
  end

  sqrt_k = Math.sqrt(k).to_i
  input_mergers = (0...sqrt_k).map { |i| inputs[i * sqrt_k] }
  input_merger_sizes = (0...sqrt_k).map { |i| input_sizes[i * sqrt_k] }
  sub_buffers = Array.new(sqrt_k) { Array.new(buffer_size, 0) }
  sub_buffer_sizes = Array.new(sqrt_k, 0)

  k32 = k ** 1.5
  (0...sqrt_k).each do |i|
    if sub_buffer_sizes[i] < k32
      temp_output = k_merger(input_mergers, input_merger_sizes, sqrt_k, sub_buffers[i], buffer_size, k32)
      sub_buffers[i][0...temp_output.size] = temp_output
      sub_buffer_sizes[i] = temp_output.size
    end
  end

  output_merger_inputs = sub_buffers
  output_merger_sizes = sub_buffer_sizes
  output = k_merger(output_merger_inputs, output_merger_sizes, sqrt_k, buffer, buffer_size, k3)

  (0...sqrt_k).each do |i|
    sub_buffers[i] = sub_buffers[i][output.size..-1] || []
    sub_buffer_sizes[i] -= output.size
  end

  output
end

def funnelsort(arr, z, l)
  n = arr.size
  if n <= z
    quicksort(arr, 0, n - 1)
    return
  end

  k = (n ** (1.0 / 3.0)).ceil
  sub_size = (n / k.to_f).ceil
  subarrays = []
  subarray_sizes = []

  puts "Sorted subarrays:"
  k.times do |i|
    size = i == k - 1 ? n - i * sub_size : sub_size
    subarray = arr[i * sub_size, size]
    quicksort(subarray, 0, size - 1)
    subarrays << subarray
    subarray_sizes << size
    puts "Subarray #{i}: #{subarray.join(' ')}"
  end

  buffer_size = 2 * (k ** 1.5).to_i
  buffer = Array.new(buffer_size, 0)
  output = k_merger(subarrays, subarray_sizes, k, buffer, buffer_size, n)

  arr[0...n] = output
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
z = 8
l = 2

puts "Initial array: #{arr.join(' ')}"

funnelsort(arr, z, l)

puts "Sorted array: #{arr.join(' ')}"
