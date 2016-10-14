import Data.List

-- tW is kinda like a combination of takeWhile and foldl
tW f g _ [] = []
tW f g v (x:xs) = if f v then x:tW f g (g x v) xs else []

res :: Int
res =  maximum . map read . filter ((==['1'..'9']) . sort) . map (\x -> concat . tW (<9) (\a b -> length a + b) 0 . map (show . (x*)) $ [1..]) $ [1..10^5::Int]

main = print  res
