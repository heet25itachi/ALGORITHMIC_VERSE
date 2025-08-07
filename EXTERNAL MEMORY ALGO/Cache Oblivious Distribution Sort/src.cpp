#include <iostream>
#include <vector>
#include <cmath>
using namespace std;

struct Bucket {
    vector<int> elements;
    int pivot;
};

void swap(int &a, int &b) { int tmp = a; a = b; b = tmp; }

int approximate_median(vector<int> &arr) {
    if (arr.empty()) return 0;
    int mid = arr.size() / 2;
    vector<int> values = {arr[0], arr[mid], arr[arr.size() - 1]};
    sort(values.begin(), values.end());
    return values[1];
}

void quicksort(vector<int> &arr, int left, int right) {
    if (left >= right) return;
    int pivot = arr[right], i = left, j = right;
    while (i < j) {
        while (i < j && arr[i] <= pivot) i++;
        while (i < j && arr[j] > pivot) j--;
        if (i < j) swap(arr[i], arr[j]);
    }
    swap(arr[i], arr[right]);
    quicksort(arr, left, i - 1);
    quicksort(arr, i + 1, right);
}

void copy_elems(vector<int> &arr, int &next, int &bnum, vector<Bucket> &buckets, int subarray_size, int bucket_idx, int sqrt_n) {
    while (next < subarray_size) {
        if (bnum >= buckets.size()) {
            buckets.push_back({vector<int>(), (int)1e9});
        }
        if (arr[next] <= buckets[bnum].pivot) {
            if (buckets[bnum].elements.size() >= 2 * sqrt_n) {
                int median = approximate_median(buckets[bnum].elements);
                buckets.push_back({vector<int>(), buckets[bnum].pivot});
                buckets[bnum].pivot = median;
                vector<int> new_elements;
                for (int x : buckets[bnum].elements)
                    if (x <= median) new_elements.push_back(x);
                    else buckets.back().elements.push_back(x);
                buckets[bnum].elements = new_elements;
                for (int &b : bnum) if (b > bnum) b++;
            }
            buckets[bnum].elements.push_back(arr[next]);
            next++;
        } else {
            bnum++;
        }
    }
}

void distribute(vector<int> &arr, vector<int> &next, vector<int> &bnum, int i, int j, int m, vector<Bucket> &buckets, int sqrt_n) {
    if (m == 1) {
        copy_elems(arr, next[i], bnum[i], buckets, sqrt_n, j, sqrt_n);
    } else {
        distribute(arr, next, bnum, i, j, m / 2, buckets, sqrt_n);
        distribute(arr, next, bnum, i + m / 2, j, m / 2, buckets, sqrt_n);
        distribute(arr, next, bnum, i, j + m / 2, m / 2, buckets, sqrt_n);
        distribute(arr, next, bnum, i + m / 2, j + m / 2, m / 2, buckets, sqrt_n);
    }
}

void cache_oblivious_sort(vector<int> &arr) {
    int n = arr.size();
    if (n <= 1) return;
    int sqrt_n = (int)sqrt(n);
    if (sqrt_n * sqrt_n != n) return;

    // Step 1: Partition and sort subarrays
    for (int i = 0; i < sqrt_n; i++)
        quicksort(arr, i * sqrt_n, i * sqrt_n + sqrt_n - 1);

    // Step 2: Distribute
    vector<int> next(sqrt_n, 0), bnum(sqrt_n, 0);
    vector<Bucket> buckets = {{vector<int>(), (int)1e9}};
    distribute(arr, next, bnum, 0, 0, sqrt_n, buckets, sqrt_n);

    // Step 3: Sort buckets
    for (auto &bucket : buckets)
        quicksort(bucket.elements, 0, bucket.elements.size() - 1);

    // Step 4: Concatenate
    int k = 0;
    for (auto &bucket : buckets)
        for (int x : bucket.elements)
            arr[k++] = x;
}

int main() {
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    cout << "Initial array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;
    cache_oblivious_sort(arr);
    cout << "Sorted array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;
    return 0;
}
