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

def pem_select(arr, n, k)
  if n <= 5
    return arr[0...n].sort[k - 1]
  end
  arr[k - 1] # Simplified: use direct access for small arrays
end

def pem_multipartition(arr, n, pivots, d_sqrt, p)
  counts = Array.new(p * d_sqrt, 0)
  (0...p).each do |i|
    start = i * (n / p)
    size = i == p - 1 ? n - start : n / p
    (0...size).each do |j|
      elem = arr[start + j]
      bucket = 0
      while bucket < d_sqrt - 1 && elem > pivots[bucket]
        bucket += 1
      end
      counts[i * d_sqrt + bucket] += 1
    end
  end

  prefix_sums = Array.new(d_sqrt, 0)
  (0...d_sqrt).each do |j|
    (0...p).each do |i|
      prefix_sums[j] += counts[i * d_sqrt + j]
    end
  end

  buckets = Array.new(d_sqrt) { [] }
  offsets = Array.new(d_sqrt, 0)
  (0...p).each do |i|
    start = i * (n / p)
    size = i == p - 1 ? n - start : n / p
    (0...size).each do |j|
      elem = arr[start + j]
      bucket = 0
      while bucket < d_sqrt - 1 && elem > pivots[bucket]
        bucket += 1
      end
      buckets[bucket][offsets[bucket]] = elem
      offsets[bucket] += 1
    end
  end

  [buckets, prefix_sums]
end

def pem_dist_sort(arr, p, m, b, d)
  n = arr.size
  if n <= m
    quicksort(arr, 0, n - 1)
    return
  end

  d_sqrt = Math.sqrt(d).to_i
  segment_size = n / p

  puts "Segments:"
  (0...p).each do |i|
    size = i == p - 1 ? n - i * segment_size : segment_size
    puts "Segment #{i}: #{arr[i * segment_size, size].join(" ")}"
  end

  pivots = (0...d_sqrt-1).map { |j| pem_select(arr, n, (j + 1) * n / d_sqrt) }
  puts "Pivots: #{pivots.join(" ")}"

  buckets, bucket_sizes = pem_multipartition(arr, n, pivots, d_sqrt, p)

  puts "Buckets:"
  (0...d_sqrt).each do |j|
    puts "Bucket #{j}: #{buckets[j].join(" ")}"
  end

  output = []
  (0...d_sqrt).each do |j|
    processors = (bucket_sizes[j].to_f / (n / p)).ceil
    pem_dist_sort(buckets[j], processors, m, b, d)
    output.concat(buckets[j])
  end

  arr[0...n] = output
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
p = 4
m = 8
b = 2
d = 4

puts "Initial array: #{arr.join(" ")}"

pem_dist_sort(arr, p, m, b, d)

puts "Sorted array: #{arr.join(" ")}"
