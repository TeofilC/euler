import Data.List
import Data.Array

f :: [[Int]] -> [Int]
f = foldl1 (\n o -> zipWith (+) (o) (map (uncurry max) $ zip n (drop 1 $ n)))

main = do
  file <- readFile "67.in"
  print . f . reverse . map (map read . words) . lines $ file
