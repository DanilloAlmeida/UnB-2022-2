{-
    RECEBE...: uma lista
    RETORNA..: o tamanho da lista
-}
size_list :: [Int] -> Int
size_list [] = 0
size_list (x:xs) = 1 + size_list xs