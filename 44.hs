{-# LANGUAGE BangPatterns #-}
import Control.Monad
pent :: Int -> Int
pent !n = n*(3*n-1)`quot`2

isPent :: Int -> Bool
isPent !n = sqrtd^2 == det && (1+sqrtd) `rem` 6 == 0
  where
    !det = 1 + 24*n
    !sqrtd = floor . sqrt . fromIntegral $ det

res :: [Int]
res = do
  a <- [1..5000]
  b <- [1..a]
  let pa = pent a
      pb = pent b
  guard (isPent $ abs $ pa - pb)
  guard (isPent $ pa + pb)
  return (pa-pb)

main = print res
