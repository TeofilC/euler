{-# LANGUAGE BangPatterns #-}
import Data.Maybe
c = 2*10^6

f x y = x*(x+1)*y*(y+1)`div`4

bin :: (Integral a) => (a -> Bool) -> (a,a) -> Maybe a
bin f (lo,hi)  | lo == hi = if f lo then Just lo else Nothing
               | otherwise= if f mid then bin f (lo,mid) else bin f (mid+1,hi)
                 where
                   !mid = lo + (hi-lo)`div`2
res :: (Int,Int)
res = minimum $  (\(x,y) -> (abs (2000000 - f x y) , x*y)) <$> ([1..1500] >>= (\x -> maybeToList (bin ((>2000000) . f x) (0,1500)) >>= (\a -> [(x,a),(x,a-1)])))

main = print res
