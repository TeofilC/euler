module Fortyeight where

-- modpow m n x = x^n mod m
modpow :: Integer -> Integer -> Integer -> Integer
modpow m x 0 = 1
modpow m x n | even n    = r*r `rem` m
             | otherwise = x * modpow m x (n-1) `rem` m
               where r = modpow m x (n `div` 2)

ans = (flip rem $ 10^10) . sum . map (\x -> modpow (10^10) x x) $ [1..1000]
