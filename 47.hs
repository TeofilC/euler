module Fortyseven where
import Control.Monad
import Data.List
primes = 2:filter (\x -> (==x) . smallestFactor $ x) [3,5..]

safeHead a [] = a
safeHead _ (x:_) = x

smallestFactor n = safeHead n . filter (\x -> n `rem` x == 0) . takeWhile (<=(floor . sqrt .fromIntegral $ n)) $ primes

factors 1 = []
factors n = sf:factors (n `div` sf)
  where sf = smallestFactor n

ans = [1..] >>= \x ->
                  mapM (guard . (>=4) . length . nub . factors) [x,x+1,x+2,x+3] >>
                  return x

main = print $ (head ans :: Int) 
