--module Fortynine where
import Control.Monad
import Data.List
primes = 2:filter isPrime [3,5..]

safeHead a [] = a
safeHead _ (x:_) = x

isPrime n = not . any (\x -> n `rem` x == 0) . takeWhile (<=(floor . sqrt .fromIntegral $ n)) $ primes


ans = do
       x <- [1000..9999]
       r <- [1..(10000-x)`div`2]
       let s = [x,x+r,x+2*r]
       mapM (guard . isPrime) s
       mapM (guard . (==(sort . show $ x)). sort .show) (tail s)
       return $ concat $ map show s
  

main = print $ (ans) 
