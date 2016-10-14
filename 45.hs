
eq l@(x:xs) r@(y:ys) | x == y    = x:(eq xs ys)
                     | x <  y    = eq xs r
                     | otherwise = eq l ys
eq _ _ = []

tria = (\x-> x*(x+1)`quot`2) <$> [1..]
pent = (\x-> x*(3*x-1)`quot`2) <$> [1..]
hexa = (\x-> x*(2*x-1)) <$> [1..]

res = (tria `eq` pent `eq` hexa) !! 2

main = print res
