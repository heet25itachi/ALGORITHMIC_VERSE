#include <iostream>

int linear_search(int arr[], int size, int target) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == target) {
            return i;
        }
    }
    return -1;
}

int main() {
    int arr[] = {3, 7, 1, 9, 4};
    int size = sizeof(arr) / sizeof(arr[0]);
    int target = 9;
    int result = linear_search(arr, size, target);
    std::cout << "Target " << target << " found at index: " << result << std::endl;
    return 0;
}
