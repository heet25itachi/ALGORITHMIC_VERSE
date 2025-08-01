def linear_search(arr, target):
    for i in range(len(arr)):
        if arr[i] == target:
            return i
    return -1

arr = [3, 7, 1, 9, 4]
target = 9
result = linear_search(arr, target)
print(f"Target {target} found at index: {result}")
