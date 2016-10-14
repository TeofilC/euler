import Control.Applicative
import qualified Data.Set as S

primes = [2,3,5] ++ (filter (\x-> all (\y-> x `rem` y /= 0) $ takeWhile (\y-> y*y <= x) primes) [7,9..])

sprime = S.fromList $ takeWhile (<10^6) primes

possible 1 = [1..9]
possible n = liftA2 (\x y -> 10*x + y) (possible $ n-1) [1,3,5,7,9]

p n = filter (flip S.member sprime) (possible n)

fR 0 = True
fR n = S.member n sprime && fR (n `div` 10)

l10 :: Double
l10 = log 10.0

fL 0 = True
fL n = S.member n sprime && fL (n `rem` 10^x)
  where
    x = floor $ log (fromIntegral n) / l10

res :: Int
res = sum $ filter (\x -> fR x && fL x) $ [2..6] >>= p

main = print res
