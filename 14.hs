import Data.Array
import Data.Foldable

col :: Int -> Int
col 1 = 1
col x | x `rem` 2 == 0 = 1 + (col1 $ x `div` 2)
      | otherwise      = 1 + (col1 $ 3*x + 1)

col1 :: Int -> Int
col1 x | x < 1000000 = a ! x
       | otherwise   = col x

a = array (1,999999) [(x,col x) | x <- [1..999999]]

res :: (Int,Int)
res = maximum . flip zip [1..999999] . toList $ a

main = print res
