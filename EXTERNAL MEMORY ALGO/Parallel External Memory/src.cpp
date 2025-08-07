#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
using namespace std;

void quicksort(vector<int>& arr, int left, int right) {
    if (left >= right) return;
    int pivot = arr[right], i = left, j = right;
    while (i < j) {
        while (i < j && arr[i] <= pivot) i++;
        while (i < j && arr[j] > pivot) j--;
        if (i < j) swap(arr[i], arr[j]);
    }
    arr[right] = arr[i]; arr[i] = pivot;
    quicksort(arr, left, i - 1);
    quicksort(arr, i + 1, right);
}

int pem_select(vector<int>& arr, int k) {
    if (arr.size() <= 5) {
        sort(arr.begin(), arr.end());
        return arr[k - 1];
    }
    return arr[k - 1]; // Simplified: use sort for small arrays
}

void pem_multipartition(vector<int>& arr, int n, vector<int>& pivots, int d_sqrt, int p, vector<vector<int>>& buckets) {
    vector<int> bucket_sizes(d_sqrt, 0);
    vector<int> counts(p * d_sqrt, 0);

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

    vector<int> prefix_sums(d_sqrt, 0);
    for (int j = 0; j < d_sqrt; j++) {
        for (int i = 0; i < p; i++) prefix_sums[j] += counts[i * d_sqrt + j];
    }

    buckets.resize(d_sqrt);
    for (int j = 0; j < d_sqrt; j++) {
        buckets[j].resize(prefix_sums[j]);
        bucket_sizes[j] = prefix_sums[j];
    }

    vector<int> offsets(d_sqrt, 0);
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
}

void pem_dist_sort(vector<int>& arr, int p, int m, int b, int d) {
    int n = arr.size();
    if (n <= m) {
        quicksort(arr, 0, n - 1);
        return;
    }

    int d_sqrt = (int)sqrt(d);
    int segment_size = n / p;

    cout << "Segments:\n";
    for (int i = 0; i < p; i++) {
        int size = i == p - 1 ? n - i * segment_size : segment_size;
        cout << "Segment " << i << ": ";
        for (int j = 0; j < size; j++) cout << arr[i * segment_size + j] << " ";
        cout << endl;
    }

    vector<int> pivots(d_sqrt - 1);
    for (int j = 0; j < d_sqrt - 1; j++) {
        pivots[j] = pem_select(arr, (j + 1) * n / d_sqrt);
    }
    cout << "Pivots: ";
    for (int x : pivots) cout << x << " ";
    cout << endl;

    vector<vector<int>> buckets;
    pem_multipartition(arr, n, pivots, d_sqrt, p, buckets);

    cout << "Buckets:\n";
    for (int j = 0; j < d_sqrt; j++) {
        cout << "Bucket " << j << ": ";
        for (int x : buckets[j]) cout << x << " ";
        cout << endl;
    }

    vector<int> output(n);
    int output_pos = 0;
    for (int j = 0; j < d_sqrt; j++) {
        int processors = (int)ceil((double)buckets[j].size() / (n / p));
        pem_dist_sort(buckets[j], processors, m, b, d);
        copy(buckets[j].begin(), buckets[j].end(), output.begin() + output_pos);
        output_pos += buckets[j].size();
    }

    arr = output;
}

int main() {
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    int n = arr.size(), p = 4, m = 8, b = 2, d = 4;

    cout << "Initial array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;

    pem_dist_sort(arr, p, m, b, d);

    cout << "Sorted array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;

    return 0;
}
