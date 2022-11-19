maxi :: Int -> Int -> Int
maxi a b | a > b = a
         | otherwise = b

maxFun :: (Int -> Int) -> Int -> Int
maxFun f 0 = f 0
maxFun f n = maxi (maxFun f(n-1)) (f n)

sales :: Int -> Int
sales n = 10 * n

maxSales :: Int -> Int
maxSales n | n == 0 = sales 0
           | n > 0 = maxi(maxSales (n-1)) (sales n)

maxSales2 :: (Int -> Int) -> Int -> Int
maxSales2 f 0 = f 0
maxSales2 f n = maxi (maxSales2 f(n-1)) (f n)

isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f n | (n > 0) && ( f n > f (n-1)) = isCrescent f (n-1)
               | n < 0 = False

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)

sumList = fold (+)

subList = fold (-)

-- SOMA MATRIZES
--Solução Guilherme
somaMatriz :: [[Int]] -> [[Int]] -> [[Int]]
somaMatriz [] [] = []
somaMatriz (x:xas) (y:yas) = zipWith (+) x y : somaMatriz xas yas

-- Solução Anderson
sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

matrixSum :: [[Int]] -> [[Int]] -> [[Int]]
matrixSum [] [] = []
matrixSum (a:as) (b:bs) = map sumPair (zip a b):matrixSum as bs


--Solução Alexandre
somaLista :: Num a => [a] -> [a] -> [a]
somaLista [] [] = []
somaLista (x:xs) (y:ys) = x + y : somaLista xs ys

somaMatriz2 :: Num a => [[a]] -> [[a]] -> [[a]]
somaMatriz2 [] [] = []
somaMatriz2 (x:xs) (y:ys) = somaLista x y : somaMatriz2 xs ys

somaLista2 lista1 lista2 = zipWith (+) lista1 lista2

somaMatriz3 :: Num a => [[a]] -> [[a]] -> [[a]]
somaMatriz3 = zipWith somaLista2