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
    arr[right], arr[i] = arr[i], pivot
    quicksort(arr, left, i - 1)
    quicksort(arr, i + 1, right)

def k_merger(inputs, input_sizes, k, buffer, buffer_size, k3):
    if k == 1:
        size = min(input_sizes[0], k3)
        output = inputs[0][:size]
        inputs[0] = inputs[0][size:]
        input_sizes[0] -= size
        return output

    sqrt_k = int(math.sqrt(k))
    input_mergers = [inputs[i * sqrt_k] for i in range(sqrt_k)]
    input_merger_sizes = [input_sizes[i * sqrt_k] for i in range(sqrt_k)]
    sub_buffers = [[] for _ in range(sqrt_k)]
    sub_buffer_sizes = [0] * sqrt_k

    k32 = int(k ** 1.5)
    for i in range(sqrt_k):
        if sub_buffer_sizes[i] < k32:
            temp_output = k_merger(input_mergers, input_merger_sizes, sqrt_k, sub_buffers[i], buffer_size, k32)
            sub_buffers[i] = temp_output
            sub_buffer_sizes[i] = len(temp_output)

    output_merger_inputs = sub_buffers
    output_merger_sizes = sub_buffer_sizes
    output = k_merger(output_merger_inputs, output_merger_sizes, sqrt_k, buffer, buffer_size, k3)

    for i in range(sqrt_k):
        sub_buffers[i] = sub_buffers[i][len(output):]
        sub_buffer_sizes[i] -= len(output)

    return output

def funnelsort(arr, z, l):
    n = len(arr)
    if n <= z:
        quicksort(arr, 0, n - 1)
        return

    k = int(math.ceil(n ** (1/3)))
    sub_size = int(math.ceil(n / k))
    subarrays = []
    subarray_sizes = []

    print("Sorted subarrays:")
    for i in range(k):
        size = n - i * sub_size if i == k - 1 else sub_size
        subarray = arr[i * sub_size:i * sub_size + size]
        quicksort(subarray, 0, len(subarray) - 1)
        subarrays.append(subarray)
        subarray_sizes.append(len(subarray))
        print(f"Subarray {i}: {' '.join(map(str, subarray))}")

    buffer_size = 2 * int(k ** 1.5)
    buffer = [0] * buffer_size
    output = k_merger(subarrays, subarray_sizes, k, buffer, buffer_size, n)

    for i in range(n):
        arr[i] = output[i]

if __name__ == "__main__":
    arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
    n, z, l = len(arr), 8, 2

    print("Initial array:", " ".join(map(str, arr)))

    funnelsort(arr, z, l)

    print("Sorted array:", " ".join(map(str, arr)))
