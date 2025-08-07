use std::f64;

struct Bucket {
    elements: Vec<i32>,
    pivot: i32,
}

fn approximate_median(arr: &Vec<i32>) -> i32 {
    if arr.is_empty() {
        return 0;
    }
    let mid = arr.len() / 2;
    let mut values = vec![arr[0], arr[mid], arr[arr.len() - 1]];
    values.sort();
    values[1]
}

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

fn copy_elems(arr: &mut [i32], next: &mut i32, bnum: &mut i32, buckets: &mut Vec<Bucket>, subarray_size: i32, bucket_idx: usize, sqrt_n: i32) {
    while *next < subarray_size {
        if *bnum >= buckets.len() as i32 {
            buckets.push(Bucket { elements: Vec::new(), pivot: 1_000_000_000 });
        }
        if arr[*next as usize] <= buckets[*bnum as usize].pivot {
            if buckets[*bnum as usize].elements.len() >= (2 * sqrt_n) as usize {
                let median = approximate_median(&buckets[*bnum as usize].elements);
                let mut new_elements = Vec::new();
                let mut new_bucket_elements = Vec::new();
                for &x in &buckets[*bnum as usize].elements {
                    if x <= median {
                        new_elements.push(x);
                    } else {
                        new_bucket_elements.push(x);
                    }
                }
                buckets[*bnum as usize].pivot = median;
                buckets[*bnum as usize].elements = new_elements;
                buckets.push(Bucket { elements: new_bucket_elements, pivot: buckets[*bnum as usize].pivot });
                for b in bnum.iter_mut() {
                    if *b > *bnum {
                        *b += 1;
                    }
                }
            }
            buckets[*bnum as usize].elements.push(arr[*next as usize]);
            *next += 1;
        } else {
            *bnum += 1;
        }
    }
}

fn distribute(arr: &mut [i32], next: &mut [i32], bnum: &mut [i32], i: i32, j: i32, m: i32, buckets: &mut Vec<Bucket>, sqrt_n: i32) {
    if m == 1 {
        copy_elems(&mut arr[(i * sqrt_n) as usize..], &mut next[i as usize], &mut bnum[i as usize], buckets, sqrt_n, i as usize, sqrt_n);
    } else {
        distribute(arr, next, bnum, i, j, m / 2, buckets, sqrt_n);
        distribute(arr, next, bnum, i + m / 2, j, m / 2, buckets, sqrt_n);
        distribute(arr, next, bnum, i, j + m / 2, m / 2, buckets, sqrt_n);
        distribute(arr, next, bnum, i + m / 2, j + m / 2, m / 2, buckets, sqrt_n);
    }
}

fn cache_oblivious_sort(arr: &mut [i32]) {
    let n = arr.len();
    if n <= 1 {
        return;
    }
    let sqrt_n = (n as f64).sqrt() as i32;
    if sqrt_n * sqrt_n != n as i32 {
        return;
    }

    // Step 1: Partition and sort subarrays
    for i in 0..sqrt_n {
        quicksort(arr, (i * sqrt_n) as usize, (i * sqrt_n + sqrt_n - 1) as usize);
    }

    // Step 2: Distribute
    let mut next = vec![0; sqrt_n as usize];
    let mut bnum = vec![0; sqrt_n as usize];
    let mut buckets = vec![Bucket { elements: Vec::new(), pivot: 1_000_000_000 }];
    distribute(arr, &mut next, &mut bnum, 0, 0, sqrt_n, &mut buckets, sqrt_n);

    // Step 3: Sort buckets
    for bucket in buckets.iter_mut() {
        quicksort(&mut bucket.elements, 0, bucket.elements.len() - 1);
    }

    // Step 4: Concatenate
    let mut k = 0;
    for bucket in buckets {
        for x in bucket.elements {
            arr[k] = x;
            k += 1;
        }
    }
}

fn main() {
    let mut arr = vec![64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
    println!("Initial array: {:?}", arr);
    cache_oblivious_sort(&mut arr);
    println!("Sorted array: {:?}", arr);
}
