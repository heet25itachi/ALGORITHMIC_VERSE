#include <stdio.h>
#include <stdlib.h>

void swap(int *a, int *b) { int tmp = *a; *a = *b; *b = tmp; }

void quicksort(int *arr, int left, int right) {
    if (left >= right) return;
    int pivot = arr[right], i = left, j = right;
    while (i < j) {
        while (i < j && arr[i] <= pivot) i++;
        while (i < j && arr[j] > pivot) j--;
        if (i < j) swap(&arr[i], &arr[j]);
    }
    swap(&arr[i], &arr[right]);
    quicksort(arr, left, i - 1);
    quicksort(arr, i + 1, right);
}

void merge_runs(int *arr, int *run1, int *run2, int run_size, int *output, int B) {
    int i = 0, j = 0, k = 0;
    while (i < run_size && j < run_size) {
        for (int b = 0; b < B && i < run_size && j < run_size; b++) {
            if (run1[i] <= run2[j]) {
                output[k++] = run1[i++];
            } else {
                output[k++] = run2[j++];
            }
        }
    }
    while (i < run_size) output[k++] = run1[i++];
    while (j < run_size) output[k++] = run2[j++];
}

void external_merge_sort(int *arr, int n, int M, int B) {
    if (n <= M) {
        quicksort(arr, 0, n - 1);
        return;
    }

    // Step 1: Divide and sort runs
    int num_runs = (n + M - 1) / M;
    int **runs = (int **)malloc(num_runs * sizeof(int *));
    for (int i = 0; i < num_runs; i++) {
        int start = i * M;
        int size = (i == num_runs - 1 && n % M != 0) ? n % M : M;
        runs[i] = (int *)malloc(size * sizeof(int));
        for (int j = 0; j < size; j++) runs[i][j] = arr[start + j];
        quicksort(runs[i], 0, size - 1);
    }

    // Print sorted runs
    printf("Sorted runs:\n");
    for (int i = 0; i < num_runs; i++) {
        printf("Run %d: ", i);
        for (int j = 0; j < (i == num_runs - 1 && n % M != 0 ? n % M : M); j++)
            printf("%d ", runs[i][j]);
        printf("\n");
    }

    // Step 2: Merge runs (2-way merge for M/B = 2)
    int *output = (int *)malloc(n * sizeof(int));
    merge_runs(arr, runs[0], runs[1], M, output, B);
    for (int i = 0; i < n; i++) arr[i] = output[i];

    // Cleanup
    for (int i = 0; i < num_runs; i++) free(runs[i]);
    free(runs);
    free(output);
}

int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    int n = 16, M = 8, B = 4;
    printf("Initial array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");
    external_merge_sort(arr, n, M, B);
    printf("Sorted array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");
    return 0;
}
