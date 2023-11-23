positivo :: Integer -> Bool
positivo x = x>=0

multiplo :: Integer -> Integer -> Bool
multiplo x y = x `mod` y == 0

enRango :: Integer -> Bool
enRango x = x >= a && x <= b
 where 
  a = 1
  b = 5

-- 2)

ptoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
ptoMedio (x1,y1) (x2,y2) = ((x1+x2)/2,(y1+y2)/2)

norma :: (Float,Float) -> Float
norma (x,y) = sqrt((x*x) + (y*y))

segundos2Tiempo :: Float -> String  --no habia necesidad de que sea float corregir
segundos2Tiempo x = 
    let h = floor (x / 3600)
        temp1 = x - fromIntegral (h * 3600)
        m = floor (temp1 / 60)   --floor convierte float a integer
        s = round (temp1 - fromIntegral (m * 60)) --round redondea a un entero
    in "De " ++ show x ++ " segundos se obtienen: " ++ show h ++ " horas, " ++ show m ++ " minutos, " ++ show s ++ " segundos."
    -- in delimita el cuerpo de la funcion


 {-  equivalente usando where  
segundos2Tiempo :: Float -> String
segundos2Tiempo x = "De " ++ show x ++ " segundos se obtienen: " ++ show h ++ " horas, " ++ show m ++ " minutos, " ++ show s ++ " segundos."
  	where
    	h = floor (x / 3600)
    	temp1 = x - fromIntegral (h * 3600)
    	m = floor (temp1 / 60)
    	s = round (temp1 - fromIntegral (m * 60))

 -}

segundosToTiempo :: Integer -> String 
segundosToTiempo x = "De " ++ show x ++ " segundos se obtienen: " ++ show h ++ " horas, " ++ show m ++ " minutos, " ++ show s ++ " segundos."
    where 
        h = fromIntegral (x `div` 3600)  -- Utiliza fromIntegral para convertir el resultado a Integer
        temp1 = x `mod` 3600
        m = fromIntegral (temp1 `div` 60)  -- Utiliza fromIntegral para convertir el resultado a Integer
        s = fromIntegral (temp1 `mod` 60)  -- Utiliza fromIntegral para convertir el resultado a Integer


--3)
limpiar :: String -> String -> String --recursivo(?)
limpiar [] _ = []  --caso base
limpiar (c:str1) str2  --(c:str1) descompone la cadena en 2 -> 1 caracter y el resto
    | elem c str2 = limpiar str1 str2  -- elem c str2 -> c pertenece a str2?  c:... agrega c a la lista devuelta por la funcion
    | otherwise = c:limpiar str1 str2  -- en otro caso, no agrega nada a la lista de retorno
  --"|" es una guardia (?) se ejecuta segun el valor  de c

--limpiar str1 str2 = filter(\c -> not(elem c str2)) str1

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:xs)
    |x == y = todosIguales(y:xs)
    |otherwise = False

repLista :: [Int] -> Int -> [Int]
repLista [] _ = []
repLista lista n = concatMap(replicate n) lista

balancedParentesis :: String -> [Char] -> Bool
balancedParentesis [] [] = True
balancedParentesis [] _ = False
balancedParentesis (x:xs) stack
    |x == '(' = balancedParentesis xs (x:stack)
    |x == ')' = case stack of
        []     -> False
        (y:ys) -> balancedParentesis xs ys 
    |otherwise = balancedParentesis xs stack


checkParentesis :: String -> Bool
checkParentesis str = balancedParentesis str []


finales :: Integer -> [Integer] -> [Integer]
finales n lista = drop(length lista - fromIntegral n)lista

--4)_
sumarCuadrados :: Integer -> Integer
sumarCuadrados 0 = 0
sumarCuadrados n = (n*n) + sumarCuadrados(n-1)

replica :: Int -> a -> [a]
replica n x
    |n <= 0 = [] 
    |otherwise = x:replica(n-1)x


-------ejercicio de practica
funcion :: Double -> Double
funcion x = exp x - log(x + 4)


biseccion :: (Double -> Double) -> (Double, Double) -> Double -> (Double, Int)
biseccion funcion (a, b) epsilon = biseccionAux funcion (a, b) epsilon 0

biseccionAux :: (Double -> Double) -> (Double, Double) -> Double -> Int -> (Double, Int)
biseccionAux funcion (a, b) epsilon contador
    | fa * fb > 0 = error "Estoy cansado jefe"
    | abs fc < epsilon = (c, contador + 1)
    | fa * fc < 0 = biseccionAux funcion (a, c) epsilon (contador + 1)
    | otherwise = biseccionAux funcion (c, b) epsilon (contador + 1)
    where
        fa = funcion a
        fb = funcion b
        fc = funcion c
        c = (a + b) / 2


transpuesta :: [[Integer]] -> [[Integer]]
transpuesta [] = []
transpuesta ([]:_) = []
transpuesta m = (map head m) : transpuesta (map tail m)

sumaMatrices :: [[Integer]] -> [[Integer]] -> [[Integer]]
sumaMatrices [] [] = []
sumaMatrices (x:xs) (c:cs) = (sumaListas x c) : sumaMatrices xs cs

sumaListas :: [Integer] -> [Integer] -> [Integer]
sumaListas [] [] = []
sumaListas (a:as) (b:bs) = (a + b):sumaListas as bs