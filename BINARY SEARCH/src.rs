fn binary_search(arr: &[i32], target: i32) -> i32 {
    let mut left = 0;
    let mut right = arr.len() as i32 - 1;
    while left <= right {
        let mid = (left + right) / 2;
        if arr[mid as usize] == target {
            return mid;
        } else if arr[mid as usize] < target {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    -1
}

fn main() {
    let arr = [1, 3, 4, 7, 9];
    let target = 9;
    let result = binary_search(&arr, target);
    println!("Target {} found at index: {}", target, result);
}
