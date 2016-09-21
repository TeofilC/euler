module Euler20 where
import Data.Char

fac 1 = 1
fac n = n*(fac $ n-1)


main = print . sum . map digitToInt . show . fac $ 100
