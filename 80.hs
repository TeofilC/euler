module Euler80 where

isInteger :: Double -> Bool
isInteger x = x == fromIntegral (floor x)

l10 :: Double
l10 = log 10

sqroot v = sqroot1 v 0 0 100

sqroot1 _ _ p  0 = p
sqroot1 v oc p n = sqroot1 v' c' p' (n-1)
  where
    f2 = 10^max 0  (floor (log (fromIntegral v) / l10)-1)
    v' = v `rem` f2
    c = oc + v `div` f2
    x = -1 + head (dropWhile (\x -> x*(20*p +x) <= c) [0..10])
    y = x*(20*p + x)
    c' = (c - y)*100
    p' = p*10 + x

fold 0 = 0
fold n = n `rem` 10 + fold (n `div` 10)

res = sum $ fold . sqroot <$> filter (not . isInteger . sqrt . fromIntegral) [1..100]


