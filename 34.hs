import Data.Char

fac 0 = 1
fac 1 = 1
fac n = n*(fac $ n-1)

res = sum $ [x | x <- [3..10000000], x == (sum $ map (fac . digitToInt) $ show x)]

main = print res
