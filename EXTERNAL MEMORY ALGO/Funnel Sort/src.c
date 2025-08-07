#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void quicksort(int *arr, int left, int right) {
    if (left >= right) return;
    int pivot = arr[right], i = left, j = right;
    while (i < j) {
        while (i < j && arr[i] <= pivot) i++;
        while (i < j && arr[j] > pivot) j--;
        if (i < j) {
            int tmp = arr[i]; arr[i] = arr[j]; arr[j] = tmp;
        }
    }
    arr[right] = arr[i]; arr[i] = pivot;
    quicksort(arr, left, i - 1);
    quicksort(arr, i + 1, right);
}

void k_merger(int **inputs, int *input_sizes, int k, int *buffer, int buffer_size, int *output, int *output_size, int k3) {
    if (k == 1) {
        int size = input_sizes[0] < k3 ? input_sizes[0] : k3;
        memcpy(output, inputs[0], size * sizeof(int));
        *output_size = size;
        memmove(inputs[0], inputs[0] + size, (input_sizes[0] - size) * sizeof(int));
        input_sizes[0] -= size;
        return;
    }

    int sqrt_k = (int)sqrt(k);
    int **input_mergers = malloc(sqrt_k * sizeof(int *));
    int *input_merger_sizes = calloc(sqrt_k, sizeof(int));
    int *sub_buffers = malloc(sqrt_k * buffer_size * sizeof(int));
    int *sub_buffer_sizes = calloc(sqrt_k, sizeof(int));

    for (int i = 0; i < sqrt_k; i++) {
        input_mergers[i] = inputs[i * sqrt_k];
        input_merger_sizes[i] = input_sizes[i * sqrt_k];
    }

    int k32 = (int)pow(k, 1.5);
    for (int i = 0; i < sqrt_k; i++) {
        if (sub_buffer_sizes[i] < k32) {
            int *temp_output = malloc(k32 * sizeof(int));
            int temp_size = 0;
            k_merger(&input_mergers[i], &input_merger_sizes[i], sqrt_k, &sub_buffers[i * buffer_size], buffer_size, temp_output, &temp_size, k32);
            memcpy(&sub_buffers[i * buffer_size], temp_output, temp_size * sizeof(int));
            sub_buffer_sizes[i] = temp_size;
            free(temp_output);
        }
    }

    int *output_merger_inputs = malloc(sqrt_k * sizeof(int *));
    int *output_merger_sizes = calloc(sqrt_k, sizeof(int));
    for (int i = 0; i < sqrt_k; i++) {
        output_merger_inputs[i] = &sub_buffers[i * buffer_size];
        output_merger_sizes[i] = sub_buffer_sizes[i];
    }

    k_merger(output_merger_inputs, output_merger_sizes, sqrt_k, buffer, buffer_size, output, output_size, k3);

    for (int i = 0; i < sqrt_k; i++) {
        memmove(&sub_buffers[i * buffer_size], &sub_buffers[i * buffer_size + *output_size], (sub_buffer_sizes[i] - *output_size) * sizeof(int));
        sub_buffer_sizes[i] -= *output_size;
    }

    free(input_mergers);
    free(input_merger_sizes);
    free(sub_buffers);
    free(sub_buffer_sizes);
    free(output_merger_inputs);
    free(output_merger_sizes);
}

void funnelsort(int *arr, int n, int z, int l) {
    if (n <= z) {
        quicksort(arr, 0, n - 1);
        return;
    }

    int k = (int)ceil(pow(n, 1.0 / 3.0));
    int sub_size = (int)ceil((double)n / k);
    int **subarrays = malloc(k * sizeof(int *));
    int *subarray_sizes = malloc(k * sizeof(int));

    printf("Sorted subarrays:\n");
    for (int i = 0; i < k; i++) {
        int size = i == k - 1 ? n - i * sub_size : sub_size;
        subarrays[i] = &arr[i * sub_size];
        subarray_sizes[i] = size;
        quicksort(subarrays[i], 0, size - 1);
        printf("Subarray %d: ", i);
        for (int j = 0; j < size; j++) printf("%d ", subarrays[i][j]);
        printf("\n");
    }

    int buffer_size = 2 * (int)pow(k, 1.5);
    int *buffer = malloc(buffer_size * sizeof(int));
    int *output = malloc(n * sizeof(int));
    int output_size = 0;

    k_merger(subarrays, subarray_sizes, k, buffer, buffer_size, output, &output_size, n);

    memcpy(arr, output, n * sizeof(int));

    free(subarrays);
    free(subarray_sizes);
    free(buffer);
    free(output);
}

int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    int n = 16, z = 8, l = 2;

    printf("Initial array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");

    funnelsort(arr, n, z, l);

    printf("Sorted array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");

    return 0;
}
