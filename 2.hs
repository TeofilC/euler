foldl1 (+) $ takeWhile (<= 4000000) $ filter even $ fmap fst $ iterate 
(\(a,b) -> (a+b,a)) (1,1)

