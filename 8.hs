readFile "8.in" >>= (return . init) >>= (\s -> return $ maximum $ [(foldl1 (*) . map (read . pure) . take 13 . drop x $ s) | x <- [0..(length s - 13)]])
