
#include <stdio.h>

int linear_search(int arr[], int size, int target) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == target) {
            return i; // Return index if found
        }
    }
    return -1; // Return -1 if not found
}

int main() {
    int arr[] = {3, 7, 1, 9, 4};
    int size = sizeof(arr) / sizeof(arr[0]);
    int target = 9;
    int result = linear_search(arr, size, target);
    printf("Target %d found at index: %d\n", target, result);
    return 0;
}
