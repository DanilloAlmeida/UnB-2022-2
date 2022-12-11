todosPares::[Int] -> Bool
todosPares [] = False
todosPares (x:xs) | (mod x 2 != 0) = False
                  | otherwise todosPares xs