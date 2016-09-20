find (\y -> (foldl1 (+) . map fromEnum $ [(rem y x == 0) | x <- [1..(floor $ sqrt $ fromIntegral y)]]) > 250) (map fst $ iterate (\(a,b) -> (a+b,b+1)) (1,2))

Alternately binary search can be used
