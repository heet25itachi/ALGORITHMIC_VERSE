fn linear_search(arr: &[i32], target: i32) -> i32 {
    for (i, &val) in arr.iter().enumerate() {
        if val == target {
            return i as i32;
        }
    }
    -1
}

fn main() {
    let arr = [3, 7, 1, 9, 4];
    let target = 9;
    let result = linear_search(&arr, target);
    println!("Target {} found at index: {}", target, result);
}
