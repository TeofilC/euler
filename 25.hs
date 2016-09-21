import Data.List


fib = map fst $ iterate (\(a,b) -> (a+b,a)) (1,0)

res = find ((>=10^999) . fst) $ zip fib [1..]

main = print res
