
-- this gives twice the actual value but that doesn't really matter
tria p = length [p | a <- [1..p`div`2-1], (p*(p -2*a)) `mod` (2*(p-a)) == 0]

res = maximum $ zip (tria <$> [1..1000]) [1..]

main = print res
