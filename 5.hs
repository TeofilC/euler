foldl (\a b -> a*b `div` (gcd a b)) 1 [1..20]
