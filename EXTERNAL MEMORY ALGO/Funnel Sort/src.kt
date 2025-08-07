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

fn k_merger(inputs: &mut [Vec<i32>], input_sizes: &mut [i32], k: i32, buffer: &mut [i32], buffer_size: i32, k3: i32) -> Vec<i32> {
    if k == 1 {
        let size = min(input_sizes[0], k3) as usize;
        let output = inputs[0][..size].to_vec();
        inputs[0].drain(..size);
        input_sizes[0] -= size as i32;
        return output;
    }

    let sqrt_k = (k as f64).sqrt() as i32;
    let mut input_mergers = vec![vec![]; sqrt_k as usize];
    let mut input_merger_sizes = vec![0; sqrt_k as usize];
    let mut sub_buffers = vec![vec![0; buffer_size as usize]; sqrt_k as usize];
    let mut sub_buffer_sizes = vec![0; sqrt_k as usize];

    for i in 0..sqrt_k as usize {
        input_mergers[i] = inputs[i * sqrt_k as usize].clone();
        input_merger_sizes[i] = input_sizes[i * sqrt_k as usize];
    }

    let k32 = (k as f64).powf(1.5) as i32;
    for i in 0..sqrt_k as usize {
        if sub_buffer_sizes[i] < k32 {
            let temp_output = k_merger(&mut input_mergers, &mut input_merger_sizes, sqrt_k, &mut sub_buffers[i], buffer_size, k32);
            sub_buffers[i][..temp_output.len()].copy_from_slice(&temp_output);
            sub_buffer_sizes[i] = temp_output.len() as i32;
        }
    }

    let mut output_merger_inputs = sub_buffers.clone();
    let mut output_merger_sizes = sub_buffer_sizes.clone();
    let output = k_merger(&mut output_merger_inputs, &mut output_merger_sizes, sqrt_k, buffer, buffer_size, k3);

    for i in 0..sqrt_k as usize {
        sub_buffers[i].drain(..output.len());
        sub_buffer_sizes[i] -= output.len() as i32;
    }

    output
}

fn funnelsort(arr: &mut [i32], z: i32, l: i32) {
    let n = arr.len() as i32;
    if n <= z {
        quicksort(arr, 0, n as usize - 1);
        return;
    }

    let k = (n as f64).powf(1.0 / 3.0).ceil() as i32;
    let sub_size = (n as f64 / k as f64).ceil() as i32;
    let mut subarrays = vec![vec![]; k as usize];
    let mut subarray_sizes = vec![0; k as usize];

    println!("Sorted subarrays:");
    for i in 0..k as usize {
        let size = if i == k as usize - 1 { n - i as i32 * sub_size } else { sub_size };
        let start = i * sub_size as usize;
        let end = start + size as usize;
        subarrays[i] = arr[start..end].to_vec();
        subarray_sizes[i] = size;
        quicksort(&mut subarrays[i], 0, size as usize - 1);
        print!("Subarray {}: ", i);
        for x in &subarrays[i] {
            print!("{} ", x);
        }
        println!();
    }

    let buffer_size = 2 * (k as f64).powf(1.5) as i32;
    let mut buffer = vec![0; buffer_size as usize];
    let output = k_merger(&mut subarrays, &mut subarray_sizes, k, &mut buffer, buffer_size, n);

    arr.copy_from_slice(&output);
}

fn main() {
    let mut arr = vec![64, 34, 25, 12, 22, 11, 90, 87, 45, 67, 23, 43, 56, 78, 91, 13];
    let n = arr.len() as i32;
    let z = 8;
    let l = 2;

    print!("Initial array: ");
    for x in &arr {
        print!("{} ", x);
    }
    println!();

    funnelsort(&mut arr, z, l);

    print!("Sorted array: ");
    for x in &arr {
        print!("{} ", x);
    }
    println!();
}
