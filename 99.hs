import Control.Arrow

--res :: String -> Int
res :: String -> Int
res = snd . maximum . flip zip [1..] . map ((\(b,e) -> e * log b) . (read *** read . tail) . break (==',')) . lines

main :: IO ()
main = do
  f <- readFile "99.in"
  print $ res f
