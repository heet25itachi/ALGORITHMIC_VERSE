#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct { int *elements; int size; int pivot; } Bucket;

void swap(int *a, int *b) { int tmp = *a; *a = *b; *b = tmp; }

int approximate_median(int *arr, int size) {
    if (size <= 0) return 0;
    int mid = size / 2;
    int indices[3] = {0, mid, size - 1};
    int values[3] = {arr[0], arr[mid], arr[size - 1]};
    for (int i = 0; i < 2; i++)
        for (int j = i + 1; j < 3; j++)
            if (values[i] > values[j]) {
                int tmp = values[i]; values[i] = values[j]; values[j] = tmp;
            }
    return values[1];
}

void quicksort(int *arr, int size) {
    if (size <= 1) return;
    int pivot = arr[size - 1], i = 0, j = size - 1;
    while (i < j) {
        while (i < j && arr[i] <= pivot) i++;
        while (i < j && arr[j] > pivot) j--;
        if (i < j) swap(&arr[i], &arr[j]);
    }
    swap(&arr[i], &arr[size - 1]);
    quicksort(arr, i);
    quicksort(arr + i + 1, size - i - 1);
}

void copy_elems(int *arr, int *next, int *bnum, Bucket *buckets, int *num_buckets, int subarray_size, int bucket_idx, int sqrt_n) {
    while (*next < subarray_size) {
        if (*bnum >= *num_buckets) {
            buckets[*num_buckets].elements = (int *)malloc(2 * sqrt_n * sizeof(int));
            buckets[*num_buckets].size = 0;
            buckets[*num_buckets].pivot = 1e9;
            (*num_buckets)++;
        }
        if (arr[*next] <= buckets[*bnum].pivot) {
            if (buckets[*bnum].size >= 2 * sqrt_n) {
                int median = approximate_median(buckets[*bnum].elements, buckets[*bnum].size);
                buckets[*num_buckets].elements = (int *)malloc(2 * sqrt_n * sizeof(int));
                buckets[*num_buckets].size = 0;
                buckets[*num_buckets].pivot = buckets[*bnum].pivot;
                buckets[*bnum].pivot = median;
                for (int i = 0; i < buckets[*bnum].size; i++)
                    if (buckets[*bnum].elements[i] > median) {
                        buckets[*num_buckets].elements[buckets[*num_buckets].size++] = buckets[*bnum].elements[i];
                        buckets[*bnum].elements[i] = 0;
                    }
                int new_size = 0;
                for (int i = 0; i < buckets[*bnum].size; i++)
                    if (buckets[*bnum].elements[i] != 0)
                        buckets[*bnum].elements[new_size++] = buckets[*bnum].elements[i];
                buckets[*bnum].size = new_size;
                for (int j = 0; j < sqrt_n; j++)
                    if (bnum[j] > *bnum) bnum[j]++;
            }
            buckets[*bnum].elements[buckets[*bnum].size++] = arr[*next];
            (*next)++;
        } else {
            (*bnum)++;
        }
    }
}

void distribute(int *arr, int *next, int *bnum, int i, int j, int m, Bucket *buckets, int *num_buckets, int sqrt_n) {
    if (m == 1) {
        copy_elems(&arr[i * sqrt_n], &next[i], &bnum[i], buckets, num_buckets, sqrt_n, j, sqrt_n);
    } else {
        distribute(arr, next, bnum, i, j, m / 2, buckets, num_buckets, sqrt_n);
        distribute(arr, next, bnum, i + m / 2, j, m / 2, buckets, num_buckets, sqrt_n);
        distribute(arr, next, bnum, i, j + m / 2, m / 2, buckets, num_buckets, sqrt_n);
        distribute(arr, next, bnum, i + m / 2, j + m / 2, m / 2, buckets, num_buckets, sqrt_n);
    }
}

void cache_oblivious_sort(int *arr, int n) {
    if (n <= 1) return;
    int sqrt_n = (int)sqrt(n);
    if (sqrt_n * sqrt_n != n) return;

    // Step 1: Partition and sort subarrays
    for (int i = 0; i < sqrt_n; i++)
        quicksort(&arr[i * sqrt_n], sqrt_n);

    // Step 2: Distribute
    int *next = (int *)calloc(sqrt_n, sizeof(int));
    int *bnum = (int *)calloc(sqrt_n, sizeof(int));
    Bucket *buckets = (Bucket *)malloc(sqrt_n * sizeof(Bucket));
    int num_buckets = 1;
    buckets[0].elements = (int *)malloc(2 * sqrt_n * sizeof(int));
    buckets[0].size = 0;
    buckets[0].pivot = 1e9;
    distribute(arr, next, bnum, 0, 0, sqrt_n, buckets, &num_buckets, sqrt_n);

    // Step 3: Sort buckets
    for (int i = 0; i < num_buckets; i++)
        quicksort(buckets[i].elements, buckets[i].size);

    // Step 4: Concatenate
    int k = 0;
    for (int i = 0; i < num_buckets; i++)
        for (int j = 0; j < buckets[i].size; j++)
            arr[k++] = buckets[i].elements[j];

    // Cleanup
    for (int i = 0; i < num_buckets; i++) free(buckets[i].elements);
    free(buckets);
    free(next);
    free(bnum);
}

int main() {
    int arr[] = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    int n = 16;
    printf("Initial array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");
    cache_oblivious_sort(arr, n);
    printf("Sorted array: ");
    for (int i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n");
    return 0;
}
