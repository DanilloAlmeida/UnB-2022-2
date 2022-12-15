lista :: [Int]
lista = [5,1,10,3,9]
lista1 :: [Int]
lista1 = [8,6,24,2,4]
lista2 :: [Int]
lista2 = [3,5,4,8,9]

getMenor :: [Int] -> Int
getMenor [x] = x
getMenor (x:xs)  | (x < getMenor xs) = x
                 | otherwise = getMenor xs

removeMenor :: [Int] -> [Int]
removeMenor []=[]
removeMenor (x:xs) | (x == getMenor (x:xs)) = xs
                   | otherwise = x:removeMenor xs

aux::[Int] -> [Int] -> [Int]
aux listaOrdenada [] = listaOrdenada
aux listaOrdenada (x:xs) = aux (listaOrdenada ++ [getMenor(x:xs)]) (removeMenor(x:xs))


ordena :: [Int] -> [Int]
ordena []=[]
ordena lista = aux [] lista




{-
aux [] [5,1,10,3,9]
    aux (  []  ++  [1])   [5,10,3,9]
        aux (   [1]  ++  [3])   [5,10,9]
            aux  ([1,3] ++ [5])   [10,9]
                aux   ([1,3,5] ++ [9])   [10]
                    aux ([1,3,5,9] ++ [10])
                        aux ([1,3,5,9] ++ [10]) ++ []
                            [1,3,5,9,10]




    aux [1] [5,10,3,9]
        aux [1,3] [5,10,9]
            aux [1,3,5]
-}


{-
ordena [5,1,10,3,9]
    1 : [5,10,3,9]




-}

{-
getMenor [5,1,10,3,9]
    5 < getMenor [1,10,3,9] ??
        1 < getMenor [10,3,9] ??
            10 < getMenor [3,9] ??
                3 < getMenor [9] ??
                    9
                3 < 9 = 3   
            10 < 3 = otherwise getMenor [3,9]
                3 < getMenor [9] ??
                    9
                3 < 9 = 3   
        1 < 3 = 1
    5 < 1 = otherwise getMenor [1,10,3,9]
        1 < getMenor [10,3,9] ??
            10 < getMenor [3,9] ??
                3 < getMenor [9] ??
                    9
                3 < 9 = 3   
            10 < 3 = otherwise getMenor [3,9]
                3 < getMenor [9] ??
                    9
                3 < 9 = 3   
1 < 3 = 1
-}