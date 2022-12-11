bigger :: [Int] -> Int
bigger [x] = x
bigger (x:xs) | (x > bigger xs) = x
              | otherwise = bigger xs


{-
bigger [1,2,3,4,5]
    1 > bigger [2,3,4,5]
        2 > bigger [3,4,5]
            3 > bigger [4,5]
                4 > bigger [5]
                    bigger [5] = 5
                4 > 5 = otherwise bigger [5] = 5
            3 > 5 = otherwise bigger [4,5]
                4 > bigger [5]
                    bigger [5] = 5
                4 > 5 = otherwise bigger [5] = 5
        2 > 5 = otherwise bigger [3,4,5] = 5
    1 > 5 = otherwise bigger [2,3,4,5] = 5
5


bigger [5,4,3,2,1]
    5 > bigger [4,3,2,1]
        4 > bigger [3,2,1]
            3 > bigger [2,1]
                2 > bigger [1]
                    bigger [1] = 1
                2 > 1 = 2
            3 > 2 = 3
        4 > 3 = 4
    5 > 4
5

bigger [1,2,5,4,3]
    1 > bigger [2,5,4,3]
        2 > bigger [5,4,3]
            5 > bigger [4,3]
                4 > bigger [3]
                    bigger [3] = 3
                4 > 3 = 4
            5 > 4 = 5
        2 > 5 = otherwise bigger[4,3]
            4 > bigger [3]
                bigger [3] = 3
            4 > 3 = 4
    1 > 4 = otherwise bigger [5,4,3]
        5 > bigger [4,3]
            4 > bigger [3]
                bigger[3]=3
            4 > 3 = 4
        5 > 4 = 5
1 > 5 = otherwise bigger [2,5,4,3]
    2 > bigger [5,4,3]
        5 > bigger [4,3]
            4 > bigger[3]
                bigger [3]=3
            4 > 3 = 4
        5 > 4 = 5
    2 > 5 = otherwise bigger [5,4,3]
        5 > bigger [4,3]
            4 > bigger [3]
                bigger [3] = 3
            4 > 3 = 4
        5 > 4 = 5
5

-}