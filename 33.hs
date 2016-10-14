import Control.Arrow
import Data.Maybe 

digs = [1..9]

f a b c = if a < b && ((a*10 + c)*b == (10*c + b)*a) then Just (a,b) else Nothing

simplify (a,b) = (a `div` g, b `div` g)
  where
    g = gcd a b

res = simplify $ product *** product $ unzip $ catMaybes $ f <$> digs <*> digs <*> digs

-- For some reason I choose not to use list comprehensions or monads for this problem
