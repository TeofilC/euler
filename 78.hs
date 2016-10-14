import Data.Array
import Data.List
import Control.Monad


sums :: Array (Int,Int) Int
sums = array ((0,0),(1000,1000)) ([((0,0),1)] ++ [((x,0),0) | x <- [1..1000]]++ [((x,y), sum . map (\a -> sums ! (x-a,y-1)). takeWhile (<=x) . map (y*) $ [0..]) | x <- [0..1000], y <- [1..1000]])

main = print $ find (\x -> sums ! (x,x-1) `rem` 10^6 == 0) [2..1000]
