fn quicksort(arr: &mut [i32], left: usize, right: usize) {
    if left >= right {
        return;
    }
    let pivot = arr[right];
    let (mut i, mut j) = (left, right);
    while i < j {
        while i < j && arr[i] <= pivot {
            i += 1;
        }
        while i < j && arr[j] > pivot {
            j -= 1;
        }
        if i < j {
            arr.swap(i, j);
        }
    }
    arr.swap(i, right);
    if i > 0 {
        quicksort(arr, left, i - 1);
    }
    quicksort(arr, i + 1, right);
}

fn merge_runs(run1: &[i32], run2: &[i32], b: usize) -> Vec<i32> {
    let mut output = Vec::with_capacity(run1.len() + run2.len());
    let mut i = 0;
    let mut j = 0;
    while i < run1.len() && j < run2.len() {
        for _ in 0..b {
            if i < run1.len() && (j >= run2.len() || run1[i] <= run2[j]) {
                output.push(run1[i]);
                i += 1;
            } else if j < run2.len() {
                output.push(run2[j]);
                j += 1;
            }
        }
    }
    output.extend_from_slice(&run1[i..]);
    output.extend_from_slice(&run2[j..]);
    return output;
}

fn external_merge_sort(arr: &mut [i32], m: usize, b: usize) {
    let n = arr.len();
    if n <= m {
        quicksort(arr, 0, n - 1);
        return;
    }

    // Step 1: Divide and sort runs
    let mut runs = Vec::new();
    for i in (0..n).step_by(m) {
        let size = if i + m > n { n - i } else { m };
        let mut run = arr[i..i + size].to_vec();
        quicksort(&mut run, 0, size - 1);
        runs.push(run);
    }

    // Print sorted runs
    println!("Sorted runs:");
    for (i, run) in runs.iter().enumerate() {
        println!("Run {}: {:?}", i, run);
    }

    // Step 2: Merge runs (2-way merge for M/B = 2)
    let output = merge_runs(&runs[0], &runs[1], b);
    arr.copy_from_slice(&output);
}

fn main() {
    let mut arr = vec![64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
    let m = 8;
    let b = 4;
    println!("Initial array: {:?}", arr);
    external_merge_sort(&mut arr, m, b);
    println!("Sorted array: {:?}", arr);
}
