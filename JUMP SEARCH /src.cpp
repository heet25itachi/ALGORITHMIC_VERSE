#include <iostream>
#include <cmath>

int min(int a, int b) {
    return a < b ? a : b;
}

int jump_search(int arr[], int size, int target) {
    int step = (int)std::sqrt(size);
    int prev = 0;
    while (arr[min(step, size) - 1] < target) {
        prev = step;
        step += (int)std::sqrt(size);
        if (prev >= size) return -1;
    }
    while (prev < size && arr[prev] < target) {
        prev++;
    }
    if (prev < size && arr[prev] == target) return prev;
    return -1;
}

int main() {
    int arr[] = {1, 3, 4, 7, 9};
    int size = sizeof(arr) / sizeof(arr[0]);
    int target = 9;
    int result = jump_search(arr, size, target);
    std::cout << "Target " << target << " found at index: " << result << std::endl;
    return 0;
}
