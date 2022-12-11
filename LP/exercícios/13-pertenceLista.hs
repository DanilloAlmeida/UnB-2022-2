belong :: [Int] -> Int -> Bool
belong [] _ = False
belong (x:xs) n | (x == n) = True
                | otherwise = belong xs n