import qualified Data.Set as Set
primes = [2,5] ++ [x | x <- [7,9..1000000], all (\y -> x `rem` y /= 0) . takeWhile (<= (floor . sqrt . fromIntegral $ x)) $ primes]

primeS = Set.fromList primes

val a b = length . takeWhile id . map (flip Set.member primeS) $ [x*x + a*x + b | x <- [0..]]

res = maximum $ do
       a <- [-1000..1000]
       b <- takeWhile (<1000) primes
       s <- [-1,1]
       return (val a (s*b), a*s*b, a , s , b)

main = print res
