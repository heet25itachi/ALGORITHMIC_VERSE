#include <iostream>

int binary_search(int arr[], int size, int target) {
    int left = 0, right = size - 1;
    while (left <= right) {
        int mid = left + (right - left) / 2;
        if (arr[mid] == target) {
            return mid;
        } else if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    return -1;
}

int main() {
    int arr[] = {1, 3, 4, 7, 9};
    int size = sizeof(arr) / sizeof(arr[0]);
    int target = 9;
    int result = binary_search(arr, size, target);
    std::cout << "Target " << target << " found at index: " << result << std::endl;
    return 0;
}
