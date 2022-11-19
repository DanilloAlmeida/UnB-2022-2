allEqual :: Int -> Int -> Int -> Bool
allEqual m n p =  (m == n) && (n == p)

casoTesteAllEqual_1 = allEqual 2 2 2 == True
casoTesteAllEqual_2 = allEqual 2 3 2 == False

resultadoCasosTesteAllEqual = foldl (&&) True [casoTesteAllEqual_1, casoTesteAllEqual_2]

maxi :: Int -> Int -> Int
maxi n m 
       | n >= m  = n
       | otherwise = m 
  
casoTesteMaxi_1 = maxi 2 3 == 3
casoTesteMaxi_2 = maxi 60 30 == 60
       
resultadoCasosTesteMaxi = foldl (&&) True [casoTesteMaxi_1, casoTesteMaxi_2]

fat ::  Int -> Int
fat  n
     | n == 0 = 1
     | n>0 = n * fat (n-1)

casoTesteFat_1 = fat 5 == 120
casoTesteFat_2 = fat 9 == 362880

resultadoCasosTesteFat = foldl (&&) True [casoTesteFat_1, casoTesteFat_2]

all4Equal :: Int -> Int -> Int -> Int -> Bool
--all4Equal a b c d  = (allEqual a b c) && (allEqual b c d)
all4Equal a b c d  = (allEqual a b c) && (c == d)
--all4Equal a b c d  =  (a == b) && (b == c) && (c == d)

casoTesteall4_1 = all4Equal 4 4 4 4 == True
casoTesteall4_2 = all4Equal 4 4 4 1 == False

resultadoCasosTesteAll4 = foldl (&&) True [casoTesteall4_1, casoTesteall4_2]

howManyEqual :: Int -> Int -> Int -> Int

howManyEqual a b c 
              | allEqual a b c = 3
              | (a == b) || (b == c) || (a == c) = 2
              | otherwise = 0

casoTesteHow_1 = howManyEqual 5 5 5 == 3
casoTesteHow_2 = howManyEqual 5 5 2 == 2

resultadoCasosTesteHow = foldl (&&) True [casoTesteHow_1, casoTesteHow_2]

{-
Defina uma função que dado um valor
inteiro s e um número de semanas n retorna
quantas semanas de 0 a n tiveram venda
igual a s.
-}

sales :: Int -> Int 
sales n = n 

funcao :: Int -> Int -> Int
funcao s 0 | sales 0 == s  = 1
           | otherwise = 0
funcao s n | n > 0 && sales(n) == s = 1 + funcao s (n-1)
           | n > 0  = funcao s (n-1)

casoTesteFsales_1 = funcao 7 3 == 0 
casoTesteFsales_2 = funcao 3 7 == 1

resultadoCasosTesteFsales = foldl (&&) True [casoTesteFsales_1, casoTesteFsales_2]

maxSales :: Int -> Int
maxSales n
         | n == 0   = sales 0
         | n > 0    = maxi (maxSales (n-1))
                           (sales n)

casoTesteMaxSales_1 = maxSales 0 == 0
casoTesteMaxSales_2 = maxSales 5 == 5

resultadoCasosTesteMaxSales = foldl (&&) True [casoTesteMaxSales_1, casoTesteMaxSales_2]

totalSales :: Int -> Int
totalSales 0 = sales 0
totalSales n
            | n > 0 = totalSales (n-1) +
                      sales n

casoTesteTSales_1 = totalSales 3 == 6
casoTesteTSales_2 = totalSales 10 == 55

resultadoCasosTesteTSales = foldl (&&) True [casoTesteTSales_1, casoTesteTSales_2]

myOr :: Bool -> Bool -> Bool
myOr True _ = True
myOr False x = x

casoTesteMyOr_1 = myOr True False == True
casoTesteMyOr_2 = myOr False False == False

resultadoCasosTesteMyOr = foldl (&&) True [casoTesteMyOr_1, casoTesteMyOr_2]

