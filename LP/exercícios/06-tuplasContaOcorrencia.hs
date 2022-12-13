type Tupla = (String, Int)
type ListaTupla = [(String, Int)]

tupla1 :: Tupla
tupla1 = ("a", 1)
tupla2 :: Tupla
tupla2 = ("b", 2)
tupla3 :: Tupla
tupla3 = ("c", 3)

listaTupla1:: ListaTupla
listaTupla1 = [tupla1, tupla2, tupla3]

conta :: Tupla -> ListaTupla -> Int 
conta _ [] = 0
conta (a,n) (x:xs) | (a == x) = 1
                   | otherwise = conta (a,n) xs