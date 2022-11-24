getEvens :: [Int] -> [Int]
getEvens [l] = filter ((==0).(mod 2))