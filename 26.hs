import qualified Data.Map.Lazy as Map
f x r = 10*r `rem` x

rems x = iterate (f x) 1

len' _ []     = 0
len' s ((x,l):xs) = if Map.member x s then
                     l - (s Map.! x)
                    else
                     len' (Map.insert x l s) xs

len x = len' Map.empty $ zip (rems x) [1..]

res = maximum . flip zip [1..] . map len $ [1..999]
