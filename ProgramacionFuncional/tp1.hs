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

--3)
limpiar :: String -> String -> String
limpiar [] _ = []
limpiar (c:str1) str2
    | elem c str2 = limpiar str1 str2
    | otherwise = c:limpiar str1 str2


--limpiar str1 str2 = filter(\c -> not(elem c str2)) str1