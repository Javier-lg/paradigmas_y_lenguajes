positivo :: Integer -> Bool
positivo x = x>=0

multiplo :: Integer -> Integer -> Bool
multiplo x y = x `mod` y == 0

enRango :: Integer -> Bool
enRango x = x >= a && x <= b
 where 
  a = 1
  b = 5