use std::cmp::min;

fn quicksort(arr: &mut [i32], left: usize, right: usize) {
    if left >= right {
        return;
    }
    let pivot = arr[right];
    let mut i = left;
    let mut j = right;
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
    arr[right] = arr[i];
    arr[i] = pivot;
    if i > 0 {
        quicksort(arr, left, i - 1);
    }
    quicksort(arr, i + 1, right);
}

fn pem_select(arr: &[i32], n: usize, k: usize) -> i32 {
    if n <= 5 {
        let mut temp = arr[..n].to_vec();
        temp.sort();
        return temp[k - 1];
    }
    arr[k - 1] // Simplified: use direct access for small arrays
}

fn pem_multipartition(arr: &[i32], n: usize, pivots: &[i32], d_sqrt: usize, p: usize) -> (Vec<Vec<i32>>, Vec<usize>) {
    let mut counts = vec![0; p * d_sqrt];
    for i in 0..p {
        let start = i * (n / p);
        let size = if i == p - 1 { n - start } else { n / p };
        for j in 0..size {
            let elem = arr[start + j];
            let mut bucket = 0;
            while bucket < d_sqrt - 1 && elem > pivots[bucket] {
                bucket += 1;
            }
            counts[i * d_sqrt + bucket] += 1;
        }
    }

    let mut prefix_sums = vec![0; d_sqrt];
    for j in 0..d_sqrt {
        for i in 0..p {
            prefix_sums[j] += counts[i * d_sqrt + j];
        }
    }

    let mut buckets = vec![vec![]; d_sqrt];
    for j in 0..d_sqrt {
        buckets[j] = vec![0; prefix_sums[j]];
    }

    let mut offsets = vec![0; d_sqrt];
    for i in 0..p {
        let start = i * (n / p);
        let size = if i == p - 1 { n - start } else { n / p };
        for j in 0..size {
            let elem = arr[start + j];
            let mut bucket = 0;
            while bucket < d_sqrt - 1 && elem > pivots[bucket] {
                bucket += 1;
            }
            buckets[bucket][offsets[bucket]] = elem;
            offsets[bucket] += 1;
        }
    }

    (buckets, prefix_sums)
}

fn pem_dist_sort(arr: &mut [i32], p: usize, m: usize, b: usize, d: usize) {
    let n = arr.len();
    if n <= m {
        quicksort(arr, 0, n - 1);
        return;
    }

    let d_sqrt = (d as f64).sqrt() as usize;
    let segment_size = n / p;

    println!("Segments:");
    for i in 0..p {
        let size = if i == p - 1 { n - i * segment_size } else { segment_size };
        print!("Segment {}: ", i);
        for j in 0..size {
            print!("{} ", arr[i * segment_size + j]);
        }
        println!();
    }

    let mut pivots = vec![0; d_sqrt - 1];
    for j in 0..d_sqrt - 1 {
        pivots[j] = pem_select(arr, n, (j + 1) * n / d_sqrt);
    }
    print!("Pivots: ");
    for x in &pivots {
        print!("{} ", x);
    }
    println!();

    let (mut buckets, bucket_sizes) = pem_multipartition(arr, n, &pivots, d_sqrt, p);

    println!("Buckets:");
    for j in 0..d_sqrt {
        print!("Bucket {}: ", j);
        for x in &buckets[j] {
            print!("{} ", x);
        }
        println!();
    }

    let mut output = vec![0; n];
    let mut output_pos = 0;
    for j in 0..d_sqrt {
        let processors = ((bucket_sizes[j] as f64) / (n as f64 / p as f64)).ceil() as usize;
        pem_dist_sort(&mut buckets[j], processors, m, b, d);
        output[output_pos..output_pos + bucket_sizes[j]].copy_from_slice(&buckets[j]);
        output_pos += bucket_sizes[j];
    }

    arr.copy_from_slice(&output);
}

fn main() {
    let mut arr = vec![64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
    let n = arr.len();
    let p = 4;
    let m = 8;
    let b = 2;
    let d = 4;

    print!("Initial array: ");
    for x in &arr {
        print!("{} ", x);
    }
    println!();

    pem_dist_sort(&mut arr, p, m, b, d);

    print!("Sorted array: ");
    for x in &arr {
        print!("{} ", x);
    }
    println!();
}
