import Data.Array

a l = array ((1,1),(20,20)) [((x,y),i) | (x,a) <- zip [1..] l, (y,i) <- zip [1..] a]

add (x,y) (a,b) = (x+a,y+b)

coords = [[(x,0) | x <- [0..3]]
         ,[(0,x) | x <- [0..3]]
         ,[(x,x) | x <- [0..3]]
         ,[(x,-x)| x <- [0..3]]]

res ar = map (product . map (ar !)) . filter (all $ inRange ((1,1),(20,20))) $ [(x,y) | x<-[1..20], y <- [1..20]] >>= (\a -> map (map $ add a) coords)

main = readFile "31.in" >>= (return . maximum . res . a . map (map read) . map words . lines)
