allEqual :: Int -> Int -> Int -> Bool
allEqual m n p =  (m == n) && (n == p)

maxi :: Int -> Int -> Int
maxi n m 
       | n >= m  = n
       | otherwise = m 
  

fat ::  Int -> Int
fat  n
     | n == 0 = 1
     | n>0 = n * fat (n-1)


all4Equal :: Int -> Int -> Int -> Int -> Bool
--all4Equal a b c d  = (allEqual a b c) && (allEqual b c d)
all4Equal a b c d  = (allEqual a b c) && (c == d)
--all4Equal a b c d  =  (a == b) && (b == c) && (c == d)

howManyEqual :: Int -> Int -> Int -> Int

howManyEqual a b c 
              | allEqual a b c = 3
              | (a == b) || (b == c) || (a == c) = 2
              | otherwise = 0

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


maxSales :: Int -> Int
maxSales n
         | n == 0   = sales 0
         | n > 0    = maxi (maxSales (n-1))
                           (sales n)

totalSales :: Int -> Int
totalSales 0 = sales 0
totalSales n
            | n > 0 = totalSales (n-1) +
                      sales n

myOr :: Bool -> Bool -> Bool
myOr True _ = True
myOr False x = x

-- pushRight 3 "hello" = "   hello" 
                         

makeSpaces :: Int -> String
--makeSpaces 3  = "   " ;
makeSpaces 0 = ""
makeSpaces n
          | n > 0 = " " ++ makeSpaces (n-1)
          
casoTesteMSpaces_1 = makeSpaces 3 == "   "
casoTesteMSpaces_2 = makeSpaces 2 == "  "

resultadoCasosTesteMSpaces = foldl (&&) True [casoTesteMSpaces_1, casoTesteMSpaces_2]

pushRight :: Int -> String -> String
pushRight n s = (makeSpaces n) ++ s

casoTestePush_1 = pushRight 3 "hello" == "   hello"
casoTestePush_2 = pushRight 2 "hello" == "  hello"
resultadoCasosTestePush = foldl (&&) True [casoTestePush_1, casoTestePush_2]          


addPair :: (Int,Int) -> Int
addPair (x,y) = x+y

double :: [Int] -> [Int]
double [] = []
double (b:bs) = (2*b) : double bs

isEven :: Int -> Bool
isEven n = mod n 2 == 0

doubleList :: [Int] -> [Int]
doubleList xs = [2*a|a <- xs]
doubleIfEven xs = [2*a|a <- xs, isEven a]

sumPairs :: [(Int,Int)] -> [Int]
sumPairs [] = []
sumPairs ((x,y):as) = (x+y) : sumPairs as 

sumPairs2 :: [(Int,Int)] -> [Int]
sumPairs2 lp = [a+b|(a,b) <- lp]

-- teste:  sumPairs [(1,2),(3,4),(5,6)] = [3,7,11]
-- sumPairs (a:as) =  (fst a) + (snd a) : sumPairs as 

member :: [Int] -> Int -> Bool
member [] _ = False
member (x:xs) n
    | n == x = True
    | otherwise = member xs n

zip2 :: [t] -> [u] -> [(t,u)]
zip2 (a:as) (b:bs) = (a,b):zip2 as bs
zip2 (a:as) [] = []
zip2 [] (b:bs) = []
zip2 [] [] = []

type Person = String
type Book = String
type Database = [(Person, Book)]

exampleBase = [("Alice", "Postman Pat"), 
               ("Anna", "All Alone"),
               ("Alice","Spot"), 
               ("Rory", "Postman Pat")]

books :: Database -> Person -> [Book]
books [] _ = []
books ((x,y):as) p 
     | x == p = y : books as p
     | otherwise = books as p 

books2 :: Database -> Person -> [Book]
books2 db p = [book | (person, book) <- db, person == p]


makeLoan  ::  Database -> Person -> Book -> Database
makeLoan db p b =  (p,b) : db

makeLoan2  ::  Database -> Person -> Book -> Database
-- cada pessoa so pode emprestar um livro no maximo uma vez
makeLoan2 [] p b = [(p,b)]
makeLoan2 ((p,l):dbs) pessoa livro 
    | (p,l) == (pessoa,livro) = dbs
    | otherwise = (p,l) : (makeLoan2 dbs pessoa livro)
    











