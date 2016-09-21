res = 1 + (sum $ take 2000 $ map snd $ iterate (\(a,b) -> (1+a,b+2*(1+ a `div` 4))) (1,3))
