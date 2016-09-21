import Data.Char
import Data.List

charToInt :: Char -> Int
charToInt c = ord c - ord 'A' + 1

main = readFile "22.in" >>= (print . sum . zipWith (*) [1..] . map (sum . map charToInt) . sort . read)
