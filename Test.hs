productoLista :: [Int] -> Int
productoLista = foldr (*) 1

sumaLista :: [Int] -> Int
sumaLista = foldr (+) 0
