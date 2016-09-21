import Data.Array
import Control.Applicative
import Data.List
import Data.Foldable

divisors a = [x | x <- [1..a-1], a `rem` x == 0]

abundant = [x | x <- [1..20161], sum (divisors x) > x]

unique [] = []
unique (a:b:xs) | a == b = unique (b:xs)
                | otherwise = a:(unique (b:xs))
unique (a:[]) = [a]

sabundant =  unique . sort . filter (<20161)$ liftA2 (+) abundant abundant

res = 28122*28123`div`2 - (sum $ sabundant)

main = print $ res

-- This is very slow. It runs in around about 20s

