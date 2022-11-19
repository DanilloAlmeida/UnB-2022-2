--1. Interseção entre duas listas:
--Solução Guilherme

contains :: Int -> [Int] -> Bool
contains _ [] = False 
contains n (x:xs) | n == x = True 
                  | otherwise = contains n xs

intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect (x:xs) y | contains x y = x : intersect xs y 
                   | otherwise = intersect xs y

inter xs ys = [a|a <- xs, a `elem` ys]

find :: Int -> [Int] -> Bool
find a [] = False
find a (b:bs) = (a == b) || find a bs

intersection :: [Int] -> [Int] -> [Int]
intersection a b = [x | x <- a, find x b]

--2. União entre duas listas (pode haver repetição):
uniao x y = x ++ y

--3. União entre duas listas (sem repetições):
listaSemR x y = tr (uniao x y)
                    where tr [] = []
                          tr (x:xs)
                                | elem x xs = tr xs
                                | otherwise = x : tr xs