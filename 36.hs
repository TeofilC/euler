import Data.List
import Data.Char
import Numeric


pal x = s == reverse s
  where
    s = show x

bpal x = s == reverse s
  where
    s = showIntAtBase 2 intToDigit x ""

res = sum [x | x <- [1..1000000], pal x && bpal x]

main = print res

