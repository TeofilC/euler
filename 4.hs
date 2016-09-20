import Control.Monad
import Data.List

isPalin a = reverse a == a

last $ sort $ [100..999] >>= (\x -> [100..999] >>= (\y -> guard (isPalin $ show $ x*y) >> return (x*y)))

