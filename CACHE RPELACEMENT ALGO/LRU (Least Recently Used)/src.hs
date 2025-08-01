module Main where

data LRUCache = LRUCache { capacity :: Int, cache :: [(Int, Int)], order :: [Int] }
  deriving Show

newCache :: Int -> LRUCache
newCache cap = LRUCache cap [] []

get :: LRUCache -> Int -> (LRUCache, Int)
get cache key =
  case lookup key (cache cache) of
    Just value ->
      let newOrder = filter (/= key) (order cache) ++ [key]
          newCache = cache { order = newOrder }
      in (newCache, value)
    Nothing -> (cache, -1)
  where
    printCache = do
      putStr $ "Cache after get(" ++ show key ++ "): ["
      sequence_ [ putStr $ "(" ++ show k ++ ", " ++ show v ++ ")" ++ if i < length (order cache) - 1 then ", " else ""
                | (i, k) <- zip [0..] (order cache), let Just v = lookup k (cache cache) ]
      putStrLn "]"

put :: LRUCache -> Int -> Int -> LRUCache
put cache key value =
  let cache' = if lookup key (cache cache) /= Nothing
               then let newOrder = filter (/= key) (order cache) ++ [key]
                    in LRUCache (capacity cache) [(k, if k == key then value else v) | (k, v) <- cache cache] newOrder
               else let newCache = if length (cache cache) == capacity cache
                                   then [(k, v) | (k, v) <- cache cache, k /= head (order cache)]
                                   else cache cache
                        newOrder = if length (cache cache) == capacity cache
                                   then tail (order cache) ++ [key]
                                   else order cache ++ [key]
                    in LRUCache (capacity cache) (newCache ++ [(key, value)]) newOrder
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
