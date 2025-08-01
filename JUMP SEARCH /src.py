import math

def jump_search(arr, target):
    size = len(arr)
    step = int(math.sqrt(size))
    prev = 0
    while arr[min(step, size) - 1] < target:
        prev = step
        step += int(math.sqrt(size))
        if prev >= size:
            return -1
    while prev < size and arr[prev] < target:
        prev += 1
    if prev < size and arr[prev] == target:
        return prev
    return -1

arr = [1, 3, 4, 7, 9]
target = 9
result = jump_search(arr, target)
print(f"Target {target} found at index: {result}")
