safediv :: Double -> Double -> Maybe Double
safediv 0 0 = Nothing
safediv 1 0 = Nothing 
safediv x y = Just (x/y)

test :: IO ()
test = do 
        putStrLn "Digite dois números"
        a <- readLn
        b <- readLn
        case safediv a b of
            Nothing -> do 
                        putStrLn "Divisão  por zero"
                        putStrLn "tente novamente"
                        test
            Just z -> putStrLn ("Resultado: " ++ show z)