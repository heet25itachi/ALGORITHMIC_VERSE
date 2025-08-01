linearSearch :: [Int] -> Int -> Int
linearSearch arr target = go arr target 0
  where
    go [] _ _ = -1
    go (x:xs) target i
      | x == target = i
      | otherwise = go xs target (i + 1)

main :: IO ()
main = do
    let arr = [3, 7, 1, 9, 4]
        target = 9
        result = linearSearch arr target
    putStrLn $ "Target " ++ show target ++ " found at index: " ++ show result
