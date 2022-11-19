--1) Implemente funções que satisfaçam a cada um dos requisitos abaixo:
--a) Retorna a diferença entre duas listas. O resultado é uma lista.
 diferencaListas lista1 lista2 = [e| e <- lista1, not (elem e lista2)]
--b) Retorna a interseção entre duas listas. O resultado é uma lista.
 intersecaoListas lista1 lista2 = [e| e <- lista1, elem e lista2]
--c) Retorna a união entre duas listas (pode haver repetição de elementos). O resultado é uma lista.
 uniaoListas lista1 lista2 = lista1 ++ lista2
--d) Retorna a união entre duas listas (não há repetição de elementos). O resultado é uma lista.
 uniaoListasSemRepeticao lista1 lista2 = removeRepeticao (uniaoListas lista1 lista2)
                                            where removeRepeticao [] = []
                                                  removeRepeticao (a:as)
                                                                     | elem a as = removeRepeticao as
                                                                     | otherwise = a : removeRepeticao as
--e) Retorna o último elemento de uma lista.
 ultimoElemento [a] = a
 ultimoElemento (a:as) = ultimoElemento as
  
--f) Retorna o n-ésimo elemento de uma lista.
 nEsimo 1 lista =  head lista
 nEsimo n (a:as) 
             | n > 1 = nEsimo (n-1) as
  
--g) Inverte uma lista.
 inverte [] = []
 inverte (a:as)  = (inverte as) ++ [a]
 
--h) Ordena uma lista em ordem descrescente, removendo as eventuais repetições de elementos.
 ordDecres [] = []
 ordDecres (a:as) = ordDecres [e| e <- as, e > a] ++ [a] ++ ordDecres [e| e <- as, e < a]
 
--i) Retorna um booleano indicando se uma lista de inteiros é decrescente ou não. Proponha 3  soluções: 
-- a) usando sort;
 ehDescrescente lista =  lista == (ordDecres lista)
--b) usando apenas recursão;
 ehDescrescente2  [a] = True
 ehDescrescente2  (a:b:xs) = a >= b && ehDescrescente2 (b:xs)
--c) usando fold, map e zip.
 ehDescrescente3 lista =  fold (&&) (map (\(a,b) -> a>=b) (zip lista (tail lista)) )

 fold f [a] = a
 fold f (a:as) = f a (fold f as)

 data Expr = Lit Int|
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr
 eval :: Expr -> Int
 eval (Lit n) = n
 eval (Add e1 e2) = (eval e1) + (eval e2)
 eval (Sub e1 e2) = (eval e1) - (eval e2)
 eval (Mul e1 e2) = (eval e1) * (eval e2)
 
 {- 3 Crie a função foldTree, que recebe uma função e uma árvore polimórfica binária como parâmetros e 
   retorna o valor resultante de acumular a aplicação dessa função por todos os nós da árvore.
-}

 foldTree f NilT = 0
 foldTree f (Node n l r) = f n ( f (foldTree f l) (foldTree f r) )

-- a solucao abaixo eh mais geral, ja que f pode retornar qualquer valor
 foldTreeGeral f NilT iv = iv
 foldTreeGeral f (Node n l r) iv = f n ( f (foldTreeGeral f l iv ) (foldTreeGeral f r iv) )

{-
4) Defina uma função que some os elementos de uma árvore binária que armazena inteiros em seus nós. R
  esolva o exerício de duas formas diferentes: ; 
-}
--a) usando a função foldTree definida acima
 somaElementosArvore arvore = foldTree (+) arvore

--b) sem usar a função foldTree
 somaElementosArvore2 NilT = 0
 somaElementosArvore2 (Node n l r) =  n + (somaElementosArvore2 l) + (somaElementosArvore2 r) 

 data Tree t = NilT |
               Node t (Tree t) (Tree t)
 exemploArvore = Node 5 (Node 4 (Node 3 NilT NilT) (Node 2 NilT NilT))
                       (Node 7 (Node 8 NilT NilT) NilT)

               
{-
5) Sendo a função addNum abaixo
addNum :: Int -> (Int -> Int)
addNum n = h
  where h m = n + m

redefina-a usando aplicação parcial de função.
 
-}

 addNum n = (n+)
 
{-
6) Resolva em Haskell o seguinte problema: a partir de duas notas das provas de cada aluno,  determinar 
a lista dos alunos aprovados, com suas respectivas médias. O resultado deve estar ordenado crescentemente pela média 
aritmética das notas. A aprovação ocorre se, e somente se, tal média é maior ou igual a cinco.
-}

 media turma = map (\(x,y) -> (y,x)) 
                  (ordenaCrescente [((n1+n2)/2, n)| (n,n1,n2) <- turma , ((n1+n2)/2)>= 5] )
                where ordenaCrescente [] = []
                      ordenaCrescente (a:as) = ordenaCrescente [e| e <- as, e < a] ++ [a] ++ ordenaCrescente [e| e <- as, e >= a]
           
 exemploturma = [("jose",7.0,9.0),("pedro",8.0,5.0),("maria",9.0,10.0),("xyz",5.0,3.0)]





