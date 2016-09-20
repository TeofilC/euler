import Data.Maybe
import Data.List

primes = [2,3,5] ++ (filter (\a -> not $ any (\b -> rem a b == 0) $ takeWhile (<=(floor $ sqrt (fromIntegral a))) primes) 
[7,9..])

f x = let sf = fromJust $ find (\a -> rem x a == 0) primes in if x == sf then x else f (x `div` sf)

