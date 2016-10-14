import Data.Char

intrt x = if p*p == x then Just p else Nothing
  where
    p = floor . sqrt . fromIntegral $ x

isTria :: Int -> Bool
isTria x = case intrt (1 + 8*x) of
             (Just d) -> (d-1) `mod` 2 == 0
             Nothing  -> False

res = length . filter (isTria . sum . map (flip (-) 64 . ord))

main = do
  f <- readFile "42.in"
  print (res . read $ "[" ++ f ++ "]")
  return ()
