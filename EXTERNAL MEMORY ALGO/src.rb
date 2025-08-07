def quicksort(arr, left, right)
  return if left >= right
  pivot = arr[right]
  i, j = left, right
  while i < j
    i += 1 while i < j && arr[i] <= pivot
    j -= 1 while i < j && arr[j] > pivot
    arr[i], arr[j] = arr[j], arr[i] if i < j
  end
  arr[i], arr[right] = arr[right], arr[i]
  quicksort(arr, left, i - 1)
  quicksort(arr, i + 1, right)
end

def merge_runs(run1, run2, b)
  output = []
  i = j = 0
  while i < run1.length && j < run2.length
    b.times do
      if i < run1.length && (j >= run2.length || run1[i] <= run2[j])
        output << run1[i]
        i += 1
      elsif j < run2.length
        output << run2[j]
        j += 1
      end
    end
  end
  output.concat(run1[i..-1]) if i < run1.length
  output.concat(run2[j..-1]) if j < run2.length
  output
end

def external_merge_sort(arr, m, b)
  n = arr.length
  if n <= m
    quicksort(arr, 0, n - 1)
    return
  end

  # Step 1: Divide and sort runs
  runs = []
  (0...n).step(m) do |i|
    size = [m, n - i].min
    run = arr[i, size].dup
    quicksort(run, 0, size - 1)
    runs << run
  end

  # Print sorted runs
  puts "Sorted runs:"
  runs.each_with_index { |run, i| puts "Run #{i}: #{run.join(' ')}" }

  # Step 2: Merge runs (2-way merge for M/B = 2)
  output = merge_runs(runs[0], runs[1], b)
  arr[0, n] = output
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
m = 8
b = 4
puts "Initial array: #{arr.join(' ')}"
external_merge_sort(arr, m, b)
puts "Sorted array: #{arr.join(' ')}"
