module Main where

data LFUCache = LFUCache { capacity :: Int, cache :: [(Int, Int)], freq :: [(Int, Int)], order :: [Int] }
  deriving Show

newCache :: Int -> LFUCache
newCache cap = LFUCache cap [] [] []

get :: LFUCache -> Int -> (LFUCache, Int)
get cache key =
  case lookup key (cache cache) of
    Just value ->
      let newFreq = [(k, if k == key then v + 1 else v) | (k, v) <- freq cache]
          newCache = cache { freq = newFreq }
      in (newCache, value)
    Nothing -> (cache, -1)
  where
    printCache = do
      putStr $ "Cache after get(" ++ show key ++ "): ["
      sequence_ [ putStr $ "(" ++ show k ++ ", " ++ show v ++ ")" ++ if i < length (order cache) - 1 then ", " else ""
                | (i, k) <- zip [0..] (order cache), let Just v = lookup k (cache cache) ]
      putStrLn "]"

put :: LFUCache -> Int -> Int -> LFUCache
put cache key value =
  let cache' = if lookup key (cache cache) /= Nothing
               then let newCache = [(k, if k == key then value else v) | (k, v) <- cache cache]
                        newFreq = [(k, if k == key then v + 1 else v) | (k, v) <- freq cache]
                    in LFUCache (capacity cache) newCache newFreq (order cache)
               else let minKey = if length (cache cache) == capacity cache
                                then let min_freq = minimum [v | (_, v) <- freq cache]
                                         candidates = [k | (k, v) <- freq cache, v == min_freq]
                                     in head [k | k <- order cache, k `elem` candidates]
                                else 0
                        newCache = if length (cache cache) == capacity cache
                                   then [(k, v) | (k, v) <- cache cache, k /= minKey]
                                   else cache cache
                        newFreq = if length (cache cache) == capacity cache
                                  then [(k, v) | (k, v) <- freq cache, k /= minKey]
                                  else freq cache
                        newOrder = if length (cache cache) == capacity cache
                                   then [k | k <- order cache, k /= minKey]
                                   else order cache
                    in LFUCache (capacity cache) (newCache ++ [(key, value)]) (newFreq ++ [(key, 1)]) (newOrder ++ [key])
  in cache' where
    printCache = do
      putStr $ "Cache after put(" ++ show key ++ ", " ++ show value ++ "): ["
      sequence_ [ putStr $ "(" ++ show k ++ ", " ++ show v ++ ")" ++ if i < length (order cache') - 1 then ", " else ""
                | (i, k) <- zip [0..] (order cache'), let Just v = lookup k (cache cache') ]
      putStrLn "]"

main :: IO ()
main = do
  let cache = newCache 3
  let cache1 = put cache 1 10
  let cache2 = put cache1 2 20
  let cache3 = put cache2 3 30
  let cache4 = put cache3 4 40
  let (cache5, value) = get cache4 2
  putStrLn $ "Get(2) = " ++ show value
  let cache6 = put cache5 5 50
  return ()
