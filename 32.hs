{-# LANGUAGE MagicHash #-}
import Control.Applicative
import Data.Set hiding (map,filter)
import Data.List

res :: Int#
res =  sum $ toList . fromList $ splitAt <$> [1..7] <*> permutations "123456789" >>= (\(l,r) ->  (flip splitAt l <$> [1..(length l)-1]) >>= (\(a,b) -> if (read a) * (read b) == read r then [read r] else []))

main = print res


-- v. slow ~ 30s
