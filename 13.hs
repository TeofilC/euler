readFile "13.in" >>= (return . foldl1 (+) . map read . lines) >>= (return . take 10 . show)

