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
{-
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento = undefined
-}
estMed1 :: EstoqueMedicamentos
estMed1 = [("med1",10), ("med2",20), ("med3",30)]
estMed2 :: EstoqueMedicamentos
estMed2 = [("med21",210), ("med22",220), ("med23",230)]