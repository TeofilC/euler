import Data.List

primes = [2] ++ (filter isPrime [3,5..])

isPrime x = all (\y-> x `rem` y /= 0) . takeWhile (\y-> y*y <=x) $ primes


res = last . sort $ [['1'..x] | x <- ['2'..'9']] >>= filter isPrime . map read . permutations  

main = print res
