{-# LANGUAGE BangPatterns #-}
import Data.List
import Data.Ratio
import Debug.Trace
import qualified Data.IntTrie as IT

primes = [2] ++ (filter (\x -> all (\y -> x `rem` y /= 0) $ takeWhile (\z-> z*z <= x)primes)[3,5..])

mpf :: Integer -> [Integer]
mpf !n = IT.apply (fmap (flip pf primes) IT.identity) n
pf !n (x:xs)
  | x > n          = []
  | x*x > n        = [n]
  | n `mod` x == 0 = {-# SCC "a" #-}(x : mpf (d n x))
  | otherwise      = {-# SCC "b" #-}(pf n xs)
  where d !n !x | n `rem` x == 0 = d (n`div`x) x
                | otherwise      = n

totient !n = n%1 * product (map (\x -> 1 - 1%x) $ pf n primes )

divs !n = totient n * (1 % (n-1))

rats = [(divs x, x) | x <- [2..]]
  

res = find (\(a,b) -> a<15499%94744)


main = print $ res rats

-- I ended up doing this half by hand
-- I realised that larger primes give worse bang for your buck. So I just took the first ten. Then I tried increasing the powers of the lower ones and removing as many of the higher ones as possible
-- I think an approach were we use the derivatives of the totient(p^x) where p is a prime and using a greedy algorithm to choose which prime we add a power to should work
