foldl1 (+) $ filter (\a -> a `rem` 5 == 0 || a `rem` 3 == 0) [1..999]
