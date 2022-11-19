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

borrowers :: Database -> Book -> [Person]
borrowers [] _ = []
borrowers ((p,cb):db) b
    | (b == cb) = p : borrowers db b
    | otherwise = borrowers db b
 
isBorrowed :: Database -> Book -> Bool 
isBorrowed [] _ = False 
isBorrowed ((_,l):dbs) livro
    | l == livro = True 
    | otherwise = isBorrowed dbs livro
    
borrowed :: Database -> Book -> Bool
borrowed db b =  (borrowers db b) /= []

--borrowed db b = not (null bs) 
--    where bs = borrowers db b
numBorrowed :: Database -> Person -> Int
numBorrowed db p =   length (books db p)


books2 :: Database -> Person -> [Book]
books2 db p = [book | (person, book) <- db, person == p]

borrowers2 :: Database -> Book -> [Person]
borrowers2 db l = [nome | (nome, livro) <- db, livro == l]


makeLoan  ::  Database -> Person -> Book -> Database
makeLoan db p b =  (p,b) : db

makeLoan2  ::  Database -> Person -> Book -> Database
-- cada pessoa so pode emprestar um livro no maximo uma vez
makeLoan2 [] p b = [(p,b)]
makeLoan2 ((p,l):dbs) pessoa livro 
    | (p,l) == (pessoa,livro) = ((p,l):dbs)
    | otherwise = (p,l) : (makeLoan2 dbs pessoa livro)

returnLoan ::  Database -> Person -> Book -> Database  
returnLoan [] _ _ = []
returnLoan ((p,l):dbs) pessoa livro 
    | (p,l) == (pessoa,livro) = (returnLoan dbs pessoa livro)
    | otherwise = (p,l) : (returnLoan dbs pessoa livro)

returnLoan2 ::  Database -> Person -> Book -> Database  
returnLoan2 db p b = [e | e <- db, e /= (p,b)]


-- quickSort [4,2,3] = [2,3,4]
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:xs) = quickSort [e | e <- xs, e <= x] ++ [x] ++ quickSort [e | e <- xs, e > x]

matriz1 :: [[Integer]]
{- 
 matriz1 (2x3) =   1 2 3
                   4 5 6
 matriz2 (2x3) =   5 6 7
                   8 9 10
 matriz1 + matriz2  (2x3) =  6 8 10
                             12 14 16 
-}
matriz1 = [ [1,2,3], [4,5,6] ]
matriz2 = [ [5,6,7], [8,9,10] ]

square :: [Int] -> [Int]
square l = map f l
   where f x = x*x
--   square l = map (\ a -> a * a) l 

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:as) = f a (fold f as)
{-
– retorna a soma dos quadrados dos itens
• folding
– manter na lista todos os itens maiores que zero.
• filtering
-}   

sumSquare :: [Int] -> Int
sumSquare l = fold (+) (square  l)


positives :: [Int] -> [Int]
positives l = filter isPositive l
     where isPositive x = x > 0
-- positives l = filter (\ e -> e > 0) l
-- positives l = [e | e <- l, e > 0]

{-
Dada uma função f do tipo t -> u -> v,
defina uma expressão da forma
(\... -> ...)
para uma função do tipo u -> t -> v que
se comporta como f mas recebe seus
argumentos na ordem inversa
-}

f :: t -> u -> v
f = undefined
g ::  u -> t -> v

g  = \ a b -> f b a 

type Name = String
type Age = Int

data People = Person Name Age

p1 = Person "Jose" 22
p2 = Person "Maria" 23

showPerson :: People -> String
showPerson (Person n a) = n ++ " -- " ++ show a

-- Tipos de dados recursivos
data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

v1 = Lit 1   -- 1 
v2 = Add (Lit 1) (Lit 2)  -- 1 + 2
v3 = Sub v2 v1 -- (1+2) - 1
v4 = Sub (Add (Lit 1) (Lit 2)) (Lit 1) --  (1+2) - 1      

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

teste1 =  eval v1 == 1
teste2 =  eval v2 == 3
teste3 =  eval v3 == 2
teste4 =  eval v4 == 2
resultadoTeste = foldl (&&) True [teste1,teste2,teste3,teste4]

-- showExpr :: Expr -> String
















