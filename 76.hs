import Data.Array
import Control.Monad

sums :: Array (Int,Int) Int
sums = array ((0,0),(100,100)) ([((0,0),1)] ++ [((x,0),0) | x <- [1..100]]++ [((x,y), sum . map (\a -> sums ! (x-a,y-1)). takeWhile (<=x) . map (([1..101]!! (y-1))*) $ [0..]) | x <- [0..100], y <- [1..100]])

main = print $ sums ! (100,99)
