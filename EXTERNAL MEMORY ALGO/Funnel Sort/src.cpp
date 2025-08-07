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

void k_merger(vector<vector<int>>& inputs, vector<int>& input_sizes, int k, vector<int>& buffer, int buffer_size, vector<int>& output, int k3) {
    if (k == 1) {
        int size = min(input_sizes[0], k3);
        output.assign(inputs[0].begin(), inputs[0].begin() + size);
        inputs[0].erase(inputs[0].begin(), inputs[0].begin() + size);
        input_sizes[0] -= size;
        return;
    }

    int sqrt_k = (int)sqrt(k);
    vector<vector<int>> input_mergers(sqrt_k);
    vector<int> input_merger_sizes(sqrt_k);
    vector<vector<int>> sub_buffers(sqrt_k, vector<int>(buffer_size));
    vector<int> sub_buffer_sizes(sqrt_k, 0);

    for (int i = 0; i < sqrt_k; i++) {
        input_mergers[i].assign(inputs[i * sqrt_k].begin(), inputs[i * sqrt_k].end());
        input_merger_sizes[i] = input_sizes[i * sqrt_k];
    }

    int k32 = (int)pow(k, 1.5);
    for (int i = 0; i < sqrt_k; i++) {
        if (sub_buffer_sizes[i] < k32) {
            vector<int> temp_output;
            k_merger(input_mergers, input_merger_sizes, sqrt_k, sub_buffers[i], buffer_size, temp_output, k32);
            sub_buffers[i].assign(temp_output.begin(), temp_output.end());
            sub_buffer_sizes[i] = temp_output.size();
        }
    }

    vector<vector<int>> output_merger_inputs(sqrt_k);
    vector<int> output_merger_sizes(sqrt_k);
    for (int i = 0; i < sqrt_k; i++) {
        output_merger_inputs[i] = sub_buffers[i];
        output_merger_sizes[i] = sub_buffer_sizes[i];
    }

    k_merger(output_merger_inputs, output_merger_sizes, sqrt_k, buffer, buffer_size, output, k3);

    for (int i = 0; i < sqrt_k; i++) {
        sub_buffers[i].erase(sub_buffers[i].begin(), sub_buffers[i].begin() + output.size());
        sub_buffer_sizes[i] -= output.size();
    }
}

void funnelsort(vector<int>& arr, int z, int l) {
    int n = arr.size();
    if (n <= z) {
        quicksort(arr, 0, n - 1);
        return;
    }

    int k = (int)ceil(pow(n, 1.0 / 3.0));
    int sub_size = (int)ceil((double)n / k);
    vector<vector<int>> subarrays(k);
    vector<int> subarray_sizes(k);

    cout << "Sorted subarrays:\n";
    for (int i = 0; i < k; i++) {
        int size = i == k - 1 ? n - i * sub_size : sub_size;
        subarrays[i].assign(arr.begin() + i * sub_size, arr.begin() + i * sub_size + size);
        subarray_sizes[i] = size;
        quicksort(subarrays[i], 0, size - 1);
        cout << "Subarray " << i << ": ";
        for (int x : subarrays[i]) cout << x << " ";
        cout << endl;
    }

    int buffer_size = 2 * (int)pow(k, 1.5);
    vector<int> buffer(buffer_size);
    vector<int> output;

    k_merger(subarrays, subarray_sizes, k, buffer, buffer_size, output, n);

    arr = output;
}

int main() {
    vector<int> arr = {64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13};
    int n = arr.size(), z = 8, l = 2;

    cout << "Initial array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;

    funnelsort(arr, z, l);

    cout << "Sorted array: ";
    for (int x : arr) cout << x << " ";
    cout << endl;

    return 0;
}
