#include <iostream>
#include <vector>
using namespace std;

void swap(int &a, int &b) { int tmp = a; a = b; b = tmp; }

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

void merge_runs(vector<int> &run1, vector<int> &run2, vector<int> &output, int B) {
    int i = 0, j = 0, k = 0;
    while (i < run1.size() && j < run2.size()) {
        for (int b = 0; b < B && i < run1.size() && j < run2.size(); b++) {
            if (run1[i] <= run2[j]) output[k++] = run1[i++];
            else output[k++] = run2[j++];
        }
    }
    while (i < run1.size()) output[k++] = run1[i++];
    while (j < run2.size()) output[k++] = run2[j++];
}

void external_merge_sort(vector<int> &arr, int M, int B) {
    int n = arr.size();
    if (n <= M) {
        quicksort(arr, 0, n - 1);
        return;
    }

    // Step 1: Divide and sort runs
    vector<vector<int>> runs;
    for (int i = 0; i < n; i += M) {
        int size = min(M, n - i);
        vector<int> run(arr.begin() + i, arr.begin() + i + size);
        quicksort(run, 0, size - 1);
        runs.push_back(run);
    }

    // Print sorted runs
    cout << "Sorted runs:\n";
    for (int i = 0; i < runs.size(); i++) {
        cout << "Run " << i << ": ";
        for (int x : runs[i]) cout << x << " ";
        cout << endl;
    }

    // Step 2: Merge runs (2-way merge for M/B = 2)
    vector<int> output(n);
    merge_runs(runs[0], runs[1], output, B);
    for (int i = 0; i < n; i++) arr[i] = output[i];
}

int main() {
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    int M = 8, B = 4;
    cout << "Initial array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;
    external_merge_sort(arr, M, B);
    cout << "Sorted array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;
    return 0;
}
