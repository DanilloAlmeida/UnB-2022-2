inv_aux :: [t] -> [t] -> [t]
inv_aux [] l = l
inv_aux (x:xs) l = inv_aux xs l++[x]

inv_list :: [t] -> [t]
inv_list [] = []
inv_list l = inv_aux l []