primes = [2,3,5] ++ (filter (\a -> not $ any (\b -> rem a b == 0) $ takeWhile (<=(floor $ sqrt (fromIntegral a))) primes) 
[7,9..])

primes !! 10000
