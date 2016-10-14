import Data.Char
-- TODO: Implement the O(log(n)) algo

champ = (concat . map show $ [1..])

takes :: [Int] -> [a] -> [a]
takes a b = takes' a b 0

takes' [] _  _ = []
takes' (x:xs) (y:ys) n | x == n    = y:takes' xs ys (n+1)
                      | otherwise = takes' (x:xs) ys (n+1)

res = product . map digitToInt . takes [10^x-1 | x<-[0..6]] $ champ
