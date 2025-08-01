import Data.List (minimum)

jumpSearch :: [Int] -> Int -> Int
jumpSearch arr target = jumpSearch' arr target 0 step
  where
    size = length arr
    step = floor (sqrt (fromIntegral size) :: Double)
    jumpSearch' arr target prev step
      | prev >= size = -1
      | arr !! (minimum [step, size] - 1) < target = jumpSearch' arr target step (step + floor (sqrt (fromIntegral size) :: Double))
      | prev < size && arr !! prev < target = jumpSearch' arr target (prev + 1) step
      | prev < size && arr !! prev == target = prev
      | otherwise = -1

main :: IO ()
main = do
    let arr = [1, 3, 4, 7, 9]
        target = 9
        result = jumpSearch arr target
    putStrLn $ "Target " ++ show target ++ " found at index: " ++ show result
