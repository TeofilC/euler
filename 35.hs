import Data.List
import qualified Data.Set as S

primes = [2,3,5] ++ [x | x <- [7,9..1000001], all (\y -> x `rem` y /= 0) $ takeWhile (<= (floor $ sqrt $ fromIntegral x)) primes]

sprimes = S.fromList primes

log10 :: Double
log10 = log 10

rotate :: Int -> Int
rotate x = 10^(floor $ log (fromIntegral x) / log10 ) *(x `rem` 10) + (x `div` 10)

--rotate x = last x : init x

rots x = x:(takeWhile (/=x) $ iterate rotate (rotate x))
--rots :: Int -> [Int]
--rots x = map read . (show x:) . takeWhile(/= (show x)) . drop 1 . iterate rotate . show $ x

--ptest x = all (\y -> x `rem` y /= 0) . takeWhile (<= (floor $ sqrt $ fromIntegral x)) $ primes
ptest = flip S.member sprimes

prots = length . filter ptest

valid x | x > 9 = (any (x `mod` 10 ==) [1,3,7,9]) && valid (x `div` 10)
        | otherwise = True


res = length $ filter (\x -> let r = rots x in length r == prots r) $ filter valid primes

main = print res
