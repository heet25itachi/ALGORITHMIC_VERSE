Bucket = Struct.new(:elements, :pivot)

def approximate_median(arr)
  return 0 if arr.empty?
  mid = arr.length / 2
  [arr[0], arr[mid], arr[-1]].sort[1]
end

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

def copy_elems(arr, next, bnum, buckets, subarray_size, bucket_idx, sqrt_n)
  while next[bucket_idx] < subarray_size
    buckets << Bucket.new([], 1_000_000_000) if bnum[bucket_idx] >= buckets.length
    if arr[next[bucket_idx]] <= buckets[bnum[bucket_idx]].pivot
      if buckets[bnum[bucket_idx]].elements.length >= 2 * sqrt_n
        median = approximate_median(buckets[bnum[bucket_idx]].elements)
        new_bucket = Bucket.new([], buckets[bnum[bucket_idx]].pivot)
        buckets[bnum[bucket_idx]].pivot = median
        new_elements = buckets[bnum[bucket_idx]].elements.select { |x| x <= median }
        new_bucket.elements = buckets[bnum[bucket_idx]].elements.select { |x| x > median }
        buckets[bnum[bucket_idx]].elements = new_elements
        buckets << new_bucket
        bnum.each_index { |i| bnum[i] += 1 if bnum[i] > bnum[bucket_idx] }
      end
      buckets[bnum[bucket_idx]].elements << arr[next[bucket_idx]]
      next[bucket_idx] += 1
    else
      bnum[bucket_idx] += 1
    end
  end
end

def distribute(arr, next, bnum, i, j, m, buckets, sqrt_n)
  if m == 1
    copy_elems(arr[i * sqrt_n, sqrt_n], next, bnum, buckets, sqrt_n, i, sqrt_n)
  else
    distribute(arr, next, bnum, i, j, m / 2, buckets, sqrt_n)
    distribute(arr, next, bnum, i + m / 2, j, m / 2, buckets, sqrt_n)
    distribute(arr, next, bnum, i, j + m / 2, m / 2, buckets, sqrt_n)
    distribute(arr, next, bnum, i + m / 2, j + m / 2, m / 2, buckets, sqrt_n)
  end
end

def cache_oblivious_sort(arr)
  n = arr.length
  return if n <= 1
  sqrt_n = Math.sqrt(n).to_i
  return if sqrt_n * sqrt_n != n

  # Step 1: Partition and sort subarrays
  (0...sqrt_n).each do |i|
    quicksort(arr, i * sqrt_n, i * sqrt_n + sqrt_n - 1)
  end

  # Step 2: Distribute
  next_arr = Array.new(sqrt_n, 0)
  bnum = Array.new(sqrt_n, 0)
  buckets = [Bucket.new([], 1_000_000_000)]
  distribute(arr, next_arr, bnum, 0, 0, sqrt_n, buckets, sqrt_n)

  # Step 3: Sort buckets
  buckets.each { |bucket| bucket.elements.sort! }

  # Step 4: Concatenate
  k = 0
  buckets.each do |bucket|
    bucket.elements.each { |x| arr[k] = x; k += 1 }
  end
end

arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
puts "Initial array: #{arr}"
cache_oblivious_sort(arr)
puts "Sorted array: #{arr}"
