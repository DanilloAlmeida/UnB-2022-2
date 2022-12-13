import System.Win32 (COORD(x))
{-
   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.
-}

type Medicamento = String
type Quantidade = Int
type Horario = Int
type EstoqueMedicamentos = [(Medicamento, Quantidade)]
type Prescricao = (Medicamento, [Horario])
type Receituario = [Prescricao]
type PlanoMedicamento = [(Horario, [Medicamento])]
type Plantao = [(Horario, [Cuidado])]
data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento

contaMedicamento :: EstoqueMedicamentos -> Int 
contaMedicamento [] = 0
contaMedicamento (x:xs) = contaMedicamento xs + 1

getLast :: EstoqueMedicamentos -> Medicamento
getLast [x] = fst(x)
getLast (_:xs) = getLast xs

removeMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
removeMedicamento med qtd [] = []
removeMedicamento med qtd (x:xs) | (fst(x) == med) = xs
                                 | otherwise = [x] ++ removeMedicamento med qtd xs

trocaCabeca :: EstoqueMedicamentos -> EstoqueMedicamentos
trocaCabeca (x:xs) = xs

incluiMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
incluiMedicamento med qtd [] = (med, qtd):[]
incluiMedicamento med qtd (x:xs) | (fst x == med) = (med, (snd x + qtd)):xs 
                                 | otherwise = [x] ++ incluiMedicamento med qtd xs

belong :: EstoqueMedicamentos -> Medicamento -> Bool 
belong [] _ = False 
belong (x:xs) med | (fst(x) == med) = True 
                  | otherwise = belong xs med

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med qtd [] = (med, qtd):[]
comprarMedicamento med qtd (x:xs) = if (belong (x:xs) med) then
                                       incluiMedicamento med qtd (x:xs)
                                    else 
                                       (med, qtd):(x:xs)
                                       --trocaCabeca (incluiMedicamento med qtd (x:xs))
{-
trocaCabeca [1,2,3,4,5]
   trocaCabeca [2,3,4,5] ++ [1]
      trocaCabeca [3,4,5] ++ [2]
         trocaCabeca [4,5] ++ [3]
            trocaCabeca [5] ++ [4]
            5
         [5] ++ [4]
      [5] ++ [4] ++ [3]
   [5] ++ [4] ++ [3] ++ [2]
[5] ++ [4] ++ [3] ++ [2] ++ [1]
-}

{-
cmed med7 1 [(med4, 10), (med6, 5), (med7, 0)]
   med7 == med4 ??
   otherwise
   [(med4, 10)] ++ cmed med7 1 [(med6, 5), (med7, 0)]
      med7 == med6 ??
      otherwise
      [(med6, 5)] ++ cmed med7 1 [(med7, 0)]
         med7 == med7 ?? = 
      [med7, (0+1)]         
   [(med6, 5)] ++ [med7, 1]
[(med4, 10)] ++ [(med6, 5)] ++ [med7, 1]


cmed med8 1 [(med4, 10), (med6, 5), (med7, 0)]
   med8 == med4 ??
   otherwise
   [(med4, 10)] ++ cmed med8 1 [(med6, 5), (med7, 0)]
      med 8 == med 6 ??
      otherwise
      [(med6, 5)] ++ cmed med8 1 [(med7, 0)]
         med8 == med7 ??
         otherwise
         [(med7, 0)] ++ cmed med8 1 []
            (med8, 1):[]
         [(med8, 1)]
      [(med7, 0)] ++ [(med8, 1)]
   [(med6, 5)] ++ [(med7, 0)] ++ [(med8, 1)]
[(med4, 10)] ++ [(med6, 5)] ++ [(med7, 0)] ++ [(med8, 1)]
-}
{-
comprarMedicamento = undefined
-}

estoque1 :: EstoqueMedicamentos
estoque1 = [(med4, 10), (med6, 5), (med7, 0)]

estoque2 :: EstoqueMedicamentos
estoque2 = [(med4, 10), (med6, 5), (med7, 10)]

estoque3 :: EstoqueMedicamentos
estoque3 = [(med4, 10), (med6, 50), (med7, 10), (med8, 20)]

med1 :: Medicamento
med1 = "Adera"

med2 :: Medicamento
med2 = "Alprazolam"

med3 :: Medicamento
med3 = "Donepezila"

med4 :: Medicamento
med4 = "Lactulona"

med5 :: Medicamento
med5 = "Mirtazapina"

med6 :: Medicamento
med6 = "Pantoprazol"

med7 :: Medicamento
med7 = "Patz"

med8 :: Medicamento
med8 = "Quetiapina"

med9 :: Medicamento
med9 = "Xarelto"