import Data.List
import Data.Char

primes :: [Int]
primes = [2,3,5,7,11,13,17]

f n = all id [((n!!(x)*100+(n!!(x+1))*10 + (n!!(x+2)))) `rem` (primes !! (x-1)) == 0| x <- [1..7]]

res :: Int
res = sum . map (read . concat . map show) . filter f . map (map digitToInt) . permutations $ ['0'..'9']

main = print res
