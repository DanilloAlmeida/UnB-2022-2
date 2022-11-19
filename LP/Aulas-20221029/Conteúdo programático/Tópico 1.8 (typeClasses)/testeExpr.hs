data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr
            

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)


-- nocao de equivalencia *semantica*, i.e., por avaliacao
instance Eq Expr where
  e1 == e2 = eval e1 == eval e2    

{- -- nocao de equivalencia estrutural
instance Eq Expr where
  Lit x == Lit y = x == y
  Add e1 e2 == Add x y = e1 == x && e2 == y
  Sub e1 e2 == Sub x y = e1 == x && e2 == y
-}

  
instance Show Expr where
  show (Lit n) = show n
  show (Add e1 e2) =  "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Sub e1 e2) =  "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
 
