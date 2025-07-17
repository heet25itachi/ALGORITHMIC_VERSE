y :: Double -> Int -> Double
y k steps = foldl (*) 1.0 [0..(fromIntegral steps - 1)] -- y(k+1) = k * y(k)

main :: IO ()
main = do
    let k = 5
    print $ "Numerical solution for y(" ++ show k ++ ") = " ++ show (y (fromIntegral k) k)
