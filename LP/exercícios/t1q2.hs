{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

myBelong :: EstoqueMedicamentos -> Medicamento -> Bool
myBelong [] _ = False
myBelong (x:xs) med | (fst(x) == med) = True
                  | otherwise = myBelong xs med

subtraiComprimido :: Medicamento -> EstoqueMedicamentos -> EstoqueMedicamentos
subtraiComprimido med [] = []
subtraiComprimido med (x:xs) | (med == fst x) = (med, (snd x)-1) : xs
                             | otherwise = [x] ++ subtraiComprimido med xs

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med (x:xs) = if (myBelong (x:xs) med) then Just
                                --((med, snd x-1) : xs)
                                (subtraiComprimido med (x:xs))
                              else 
                                Nothing
{-
tomarMedicamento med (x:xs) | 
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

type Medicamento = String
type Quantidade = Int
type Horario = Int
type EstoqueMedicamentos = [(Medicamento, Quantidade)]
type Prescricao = (Medicamento, [Horario])
type Receituario = [Prescricao]
type PlanoMedicamento = [(Horario, [Medicamento])]
type Plantao = [(Horario, [Cuidado])]
data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento