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

def merge_runs(run1, run2, B):
    output = []
    i = j = 0
    while i < len(run1) and j < len(run2):
        for _ in range(B):
            if i < len(run1) and (j >= len(run2) or run1[i] <= run2[j]):
                output.append(run1[i])
                i += 1
            elif j < len(run2):
                output.append(run2[j])
                j += 1
    output.extend(run1[i:])
    output.extend(run2[j:])
    return output

def external_merge_sort(arr, M, B):
    n = len(arr)
    if n <= M:
        quicksort(arr, 0, n - 1)
        return

    # Step 1: Divide and sort runs
    runs = []
    for i in range(0, n, M):
        run = arr[i:i + M]
        quicksort(run, 0, len(run) - 1)
        runs.append(run)

    # Print sorted runs
    print("Sorted runs:")
    for i, run in enumerate(runs):
        print(f"Run {i}: {run}")

    # Step 2: Merge runs (2-way merge for M/B = 2)
    output = merge_runs(runs[0], runs[1], B)
    for i in range(n):
        arr[i] = output[i]

if __name__ == "__main__":
    arr = [64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13]
    M, B = 8, 4
    print("Initial array:", arr)
    external_merge_sort(arr, M, B)
    print("Sorted array:", arr)
