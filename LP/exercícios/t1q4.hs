{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}

contaPrescricao :: Prescricao -> Int
contaPrescricao (med, []) = 0
contaPrescricao (med, (x:xs)) = contaPrescricao (med, xs)+1

-- Dada uma prescrição, retorna o nome do medicamento
getNomeMed :: Prescricao -> String
getNomeMed (med, _) = med

-- Dado um receituario, retorna uma lista de tuplas
--    com nome e quantidade de comprimidos utilizados
-- getDemanda :: Receituario -> [(String, Int)]
-- getDemanda :: Receituario -> EstoqueMedicamentos
-- getDemanda [] = []
-- getDemanda (x:xs) = (getNomeMed x, contaPrescricao x) : getDemanda xs

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos [] = []
demandaMedicamentos (x:xs) = (getNomeMed x, contaPrescricao x) : demandaMedicamentos xs


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

presc1 :: Prescricao
presc1 = (med1, [0, 12])

presc2 :: Prescricao
presc2 = (med2, [0, 8, 16])

presc3 :: Prescricao
presc3 = (med3, [0, 6, 12, 18])

recei1 :: Receituario
recei1 = [presc1]

recei2 :: Receituario
recei2 = [presc1, presc2]

recei3 :: Receituario
recei3 = [presc1, presc2, presc3]

type Medicamento = String
type Quantidade = Int
type Horario = Int
type EstoqueMedicamentos = [(Medicamento, Quantidade)]
type Prescricao = (Medicamento, [Horario])
type Receituario = [Prescricao]
type PlanoMedicamento = [(Horario, [Medicamento])]
type Plantao = [(Horario, [Cuidado])]
data Cuidado = Comprar Medicamento Quantidade | Medicar Medicamento