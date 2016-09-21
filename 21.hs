module Main where
import Data.Foldable
import Data.Array

divisors x = filter (\y -> x `rem` y == 0) [1..x-1]

ar = array (0,9999) [(x,sum $ divisors x) | x <- [0..9999]]

res = sum . map fst . filter (\(a,b) -> a /= b &&  b < 10000 && (ar ! b) == a) . zip [0..] . toList $ ar


main = print res
