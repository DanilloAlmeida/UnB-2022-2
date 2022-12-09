comp_listas :: [Int] -> [Int] -> Bool 
comp_listas [] [] = True 
comp_listas [] _ = False 
comp_listas _ [] = False 
comp_listas (a:as) (b:bs)   | (a==b) = comp_listas as bs
                            | otherwise = False