import Data.Array
import Control.Monad

coins = [1,2,5,10,20,50,100,200]

sums = array ((0,0),(200,8)) ([((0,0),1)] ++ [((x,0),0) | x <- [1..200]]++ [((x,y), sum . map (\a -> sums ! (x-a,y-1)). takeWhile (<=x) . map ((coins !! (y-1))*) $ [0..]) | x <- [0..200], y <- [1..8]])
