import math

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
    arr[right], arr[i] = arr[i], arr[right]
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)

def pem_select(arr, n, k):
    if n <= 5:
        temp = sorted(arr[:n])
        return temp[k - 1]
    return arr[k - 1]  # Simplified: use sort for small arrays

def pem_multipartition(arr, n, pivots, d_sqrt, p):
    counts = [0] * (p * d_sqrt)
    for i in range(p):
        start = i * (n // p)
        size = n - start if i == p - 1 else n // p
        for j in range(size):
            elem = arr[start + j]
            bucket = 0
            while bucket < d_sqrt - 1 and elem > pivots[bucket]:
                bucket += 1
            counts[i * d_sqrt + bucket] += 1

    prefix_sums = [0] * d_sqrt
    for j in range(d_sqrt):
        for i in range(p):
            prefix_sums[j] += counts[i * d_sqrt + j]

    buckets = [[] for _ in range(d_sqrt)]
    for j in range(d_sqrt):
        buckets[j] = [0] * prefix_sums[j]

    offsets = [0] * d_sqrt
    for i in range(p):
        start = i * (n // p)
        size = n - start if i == p - 1 else n // p
        for j in range(size):
            elem = arr[start + j]
            bucket = 0
            while bucket < d_sqrt - 1 and elem > pivots[bucket]:
                bucket += 1
            buckets[bucket][offsets[bucket]] = elem
            offsets[bucket] += 1

    return buckets, prefix_sums

def pem_dist_sort(arr, p, m, b, d):
    n = len(arr)
    if n <= m:
        quicksort(arr, 0, n - 1)
        return

    d_sqrt = int(math.sqrt(d))
    segment_size = n // p

    print("Segments:")
    for i in range(p):
        size = n - i * segment_size if i == p - 1 else segment_size
        print(f"Segment {i}: {' '.join(map(str, arr[i * segment_size:i * segment_size + size]))}")

    pivots = [pem_select(arr, n, (j + 1) * n // d_sqrt) for j in range(d_sqrt - 1)]
    print("Pivots:", ' '.join(map(str, pivots)))

    buckets, bucket_sizes = pem_multipartition(arr, n, pivots, d_sqrt, p)

    print("Buckets:")
    for j in range(d_sqrt):
        print(f"Bucket {j}: {' '.join(map(str, buckets[j]))}")

    output = []
    for j in range(d_sqrt):
        processors = math.ceil(bucket_sizes[j] / (n / p))
        pem_dist_sort(buckets[j], processors, m, b, d)
        output.extend(buckets[j])

    for i in range(n):
        arr[i] = output[i]

if __name__ == "__main__":
    arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
    n, p, m, b, d = len(arr), 4, 8, 2, 4

    print("Initial array:", ' '.join(map(str, arr)))

    pem_dist_sort(arr, p, m, b, d)

    print("Sorted array:", ' '.join(map(str, arr)))
