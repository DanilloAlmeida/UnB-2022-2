pessoa :: (String, String, Double)
pessoa = ("Danillo", "casado", 13000.00)

selec_nome(x, _, _) = x
selec_estado(_, y, _) = y
selec_salario(_, _, z) = z