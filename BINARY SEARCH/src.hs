binarySearch :: [Int] -> Int -> Int
binarySearch arr target = binarySearch' arr target 0 (length arr - 1)
  where
    binarySearch' arr target left right
      | left > right = -1
      | arr !! mid == target = mid
      | arr !! mid < target = binarySearch' arr target (mid + 1) right
      | otherwise = binarySearch' arr target left (mid - 1)
      where mid = (left + right) `div` 2

main :: IO ()
main = do
    let arr = [1, 3, 4, 7, 9]
        target = 9
        result = binarySearch arr target
    putStrLn $ "Target " ++ show target ++ " found at index: " ++ show result
