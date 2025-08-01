fn min(a: i32, b: i32) -> i32 {
    if a < b { a } else { b }
}

fn jump_search(arr: &[i32], target: i32) -> i32 {
    let size = arr.len() as i32;
    let step = (size as f64).sqrt() as i32;
    let mut prev = 0;
    while arr[min(step, size) - 1] < target {
        prev = step;
        step += (size as f64).sqrt() as i32;
        if prev >= size {
            return -1;
        }
    }
    while prev < size && arr[prev as usize] < target {
        prev += 1;
    }
    if prev < size && arr[prev as usize] == target {
        return prev;
    }
    -1
}

fn main() {
    let arr = [1, 3, 4, 7, 9];
    let target = 9;
    let result = jump_search(&arr, target);
    println!("Target {} found at index: {}", target, result);
}
