--module Fifty where
import Control.Monad
import Data.List
primes = 2:filter isPrime [3,5..]

safeHead a [] = a
safeHead _ (x:_) = x

isPrime n = not . any (\x -> n `rem` x == 0) . takeWhile (<=(floor . sqrt .fromIntegral $ n)) $ primes

sums = takeWhile (<=10^6) $ scanl (+) 0 primes

difs = filter (>0) $ liftM2 (-) sums sums

ans = maximum . filter isPrime $ difs

main = print (ans :: Int)
