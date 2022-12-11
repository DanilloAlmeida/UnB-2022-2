inv_aux :: [t] -> [t] -> [t]
inv_aux [] l = l
inv_aux (x:xs) l = inv_aux xs l++[x]

inv_list :: [t] -> [t]
inv_list [] = []
inv_list l = inv_aux l []

inv_melhor :: [t] -> [t]
inv_melhor [] = []
inv_melhor (x:xs) = inv_melhor xs ++ [x]

{-
inv_melhor [1,2,3,4,5]
    inv_melhor [2,3,4,5] ++ [1]
        inv_melhor [3,4,5] ++ [2] 
            inv_melhor [4,5] ++ [3]
                inv_melhor [5] ++ [4]
                    inv_melhor [] ++ [5]
                        inv_melhor = []
                    [] = []+[5] = [5]
                [5]+[4]=[5,4]
            [5]+[4]+[3]=[5,4,3]
        [5]+[4]+[3]+[2]=[5,4,3,2]
    [5]+[4]+[3]+[2]+[1]=[5,4,3,2,1]
[5,4,3,2,1]    
-}
