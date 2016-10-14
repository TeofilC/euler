import qualified Data.Set as S

primes = 2:(filter (\x -> all (\y -> x `rem` y /= 0) . takeWhile (\y -> y*y <= x) $ primes) [3,5..])

sprime = S.fromList $ take 10000 primes
isPrime = flip S.member sprime

squares = (^2) <$> [1..]

res = head [x | x <- [3,5..], not $ any (\y-> isPrime $ x - 2*y) $ takeWhile (<=x) (0:squares)]

main = print res
