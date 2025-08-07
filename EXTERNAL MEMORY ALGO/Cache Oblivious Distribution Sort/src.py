from math import sqrt

class Bucket:
    def __init__(self):
        self.elements = []
        self.pivot = float('inf')

def approximate_median(arr):
    if not arr:
        return 0
    mid = len(arr) // 2
    values = [arr[0], arr[mid], arr[-1]]
    return sorted(values)[1]

def quicksort(arr, left, right):
    if left >= right:
        return
    pivot = arr[right]
    i, j = left, right
    while i < j:
        while i < j and arr[i] <= pivot:
            i += 1
        while i < j and arr[j] > pivot:
            j -= 1
        if i < j:
            arr[i], arr[j] = arr[j], arr[i]
    arr[i], arr[right] = arr[right], arr[i]
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)

def copy_elems(arr, next, bnum, buckets, subarray_size, bucket_idx, sqrt_n):
    while next[bucket_idx] < subarray_size:
        if bnum[bucket_idx] >= len(buckets):
            buckets.append(Bucket())
        if arr[next[bucket_idx]] <= buckets[bnum[bucket_idx]].pivot:
            if len(buckets[bnum[bucket_idx]].elements) >= 2 * sqrt_n:
                median = approximate_median(buckets[bnum[bucket_idx]].elements)
                new_bucket = Bucket()
                new_bucket.pivot = buckets[bnum[bucket_idx]].pivot
                buckets[bnum[bucket_idx]].pivot = median
                new_elements = [x for x in buckets[bnum[bucket_idx]].elements if x <= median]
                new_bucket.elements = [x for x in buckets[bnum[bucket_idx]].elements if x > median]
                buckets[bnum[bucket_idx]].elements = new_elements
                buckets.append(new_bucket)
                for i in range(len(bnum)):
                    if bnum[i] > bnum[bucket_idx]:
                        bnum[i] += 1
            buckets[bnum[bucket_idx]].elements.append(arr[next[bucket_idx]])
            next[bucket_idx] += 1
        else:
            bnum[bucket_idx] += 1

def distribute(arr, next, bnum, i, j, m, buckets, sqrt_n):
    if m == 1:
        copy_elems(arr, next, bnum, buckets, sqrt_n, i, sqrt_n)
    else:
        distribute(arr, next, bnum, i, j, m // 2, buckets, sqrt_n)
        distribute(arr, next, bnum, i + m // 2, j, m // 2, buckets, sqrt_n)
        distribute(arr, next, bnum, i, j + m // 2, m // 2, buckets, sqrt_n)
        distribute(arr, next, bnum, i + m // 2, j + m // 2, m // 2, buckets, sqrt_n)

def cache_oblivious_sort(arr):
    n = len(arr)
    if n <= 1:
        return
    sqrt_n = int(sqrt(n))
    if sqrt_n * sqrt_n != n:
        return

    # Step 1: Partition and sort subarrays
    for i in range(sqrt_n):
        quicksort(arr, i * sqrt_n, i * sqrt_n + sqrt_n - 1)

    # Step 2: Distribute
    next = [0] * sqrt_n
    bnum = [0] * sqrt_n
    buckets = [Bucket()]
    distribute(arr, next, bnum, 0, 0, sqrt_n, buckets, sqrt_n)

    # Step 3: Sort buckets
    for bucket in buckets:
        bucket.elements.sort()

    # Step 4: Concatenate
    k = 0
    for bucket in buckets:
        for x in bucket.elements:
            arr[k] = x
            k += 1

if __name__ == "__main__":
    arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
    print("Initial array:", arr)
    cache_oblivious_sort(arr)
    print("Sorted array:", arr)
