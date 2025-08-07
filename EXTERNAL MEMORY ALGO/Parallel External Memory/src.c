#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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

int pem_select(int *arr, int n, int k) {
    if (n <= 5) {
        quicksort(arr, 0, n - 1);
        return arr[k - 1];
    }
    return arr[k - 1]; // Simplified: use quicksort for small arrays
}

void pem_multipartition(int *arr, int n, int *pivots, int d_sqrt, int p, int **buckets, int *bucket_sizes) {
    for (int i = 0; i < d_sqrt; i++) bucket_sizes[i] = 0;
    int *counts = malloc(p * d_sqrt * sizeof(int));
    for (int i = 0; i < p * d_sqrt; i++) counts[i] = 0;

    for (int i = 0; i < p; i++) {
        int start = i * (n / p);
        int size = i == p - 1 ? n - start : n / p;
        for (int j = 0; j < size; j++) {
            int elem = arr[start + j];
            int bucket = 0;
            while (bucket < d_sqrt - 1 && elem > pivots[bucket]) bucket++;
            counts[i * d_sqrt + bucket]++;
        }
    }

    int *prefix_sums = malloc(d_sqrt * sizeof(int));
    for (int j = 0; j < d_sqrt; j++) {
        prefix_sums[j] = 0;
        for (int i = 0; i < p; i++) prefix_sums[j] += counts[i * d_sqrt + j];
    }

    for (int j = 0; j < d_sqrt; j++) {
        buckets[j] = malloc(prefix_sums[j] * sizeof(int));
        bucket_sizes[j] = prefix_sums[j];
    }

    int *offsets = malloc(d_sqrt * sizeof(int));
    for (int j = 0; j < d_sqrt; j++) offsets[j] = 0;

    for (int i = 0; i < p; i++) {
        int start = i * (n / p);
        int size = i == p - 1 ? n - start : n / p;
        for (int j = 0; j < size; j++) {
            int elem = arr[start + j];
            int bucket = 0;
            while (bucket < d_sqrt - 1 && elem > pivots[bucket]) bucket++;
            buckets[bucket][offsets[bucket]++] = elem;
        }
    }

    free(counts);
    free(prefix_sums);
    free(offsets);
}

void pem_dist_sort(int *arr, int n, int p, int m, int b, int d) {
    if (n <= m) {
        quicksort(arr, 0, n - 1);
        return;
    }

    int d_sqrt = (int)sqrt(d);
    int *segments = arr;
    int segment_size = n / p;

    printf("Segments:\n");
    for (int i = 0; i < p; i++) {
        int size = i == p - 1 ? n - i * segment_size : segment_size;
        printf("Segment %d: ", i);
        for (int j = 0; j < size; j++) printf("%d ", arr[i * segment_size + j]);
        printf("\n");
    }

    int *pivots = malloc((d_sqrt - 1) * sizeof(int));
    for (int j = 0; j < d_sqrt - 1; j++) {
        pivots[j] = pem_select(arr, n, (j + 1) * n / d_sqrt);
    }
    printf("Pivots: ");
    for (int j = 0; j < d_sqrt - 1; j++) printf("%d ", pivots[j]);
    printf("\n");

    int **buckets = malloc(d_sqrt * sizeof(int *));
    int *bucket_sizes = malloc(d_sqrt * sizeof(int));
    pem_multipartition(arr, n, pivots, d_sqrt, p, buckets, bucket_sizes);

    printf("Buckets:\n");
    for (int j = 0; j < d_sqrt; j++) {
        printf("Bucket %d: ", j);
        for (int k = 0; k < bucket_sizes[j]; k++) printf("%d ", buckets[j][k]);
        printf("\n");
    }

    int *output = malloc(n * sizeof(int));
    int output_pos = 0;
    for (int j = 0; j < d_sqrt; j++) {
        int processors = (int)ceil((double)bucket_sizes[j] / (n / p));
        pem_dist_sort(buckets[j], bucket_sizes[j], processors, m, b, d);
        memcpy(output + output_pos, buckets[j], bucket_sizes[j] * sizeof(int));
        output_pos += bucket_sizes[j];
        free(buckets[j]);
    }

    memcpy(arr, output, n * sizeof(int));
    free(pivots);
    free(buckets);
    free(bucket_sizes);
    free(output);
}

int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    int n = 16, p = 4, m = 8, b = 2, d = 4;

    printf("Initial array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");

    pem_dist_sort(arr, n, p, m, b, d);

    printf("Sorted array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");

    return 0;
}
