lista::[Int]
lista = [1,2,3,4,5]

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs) ++ [x]

{-
inverte [1,2,3,4,5]
    inverte [2,3,4,5] ++ [1]
        inverte [3,4,5] ++ [2]
            inverte [4,5] ++ [3]
                inverte [5] ++ [4]
                    inverte [] ++ [5]
                    []
                [5]
            [5,4]   
        [5,4,3]
    [5,4,3,2]
[5,4,3,2,1]
-}