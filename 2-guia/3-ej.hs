{-
Ejercicio 3. Especificar e implementar la funci´on esDivisible :: Integer ->Integer ->Bool que dados dos n´umeros
naturales determinar si el primero es divisible por el segundo. No est´a permitido utilizar las funciones mod ni div.
-}

esDivisible :: Integer -> Integer -> Bool
esDivisible x y
  | x < y  = False
  | x == y = True
  | otherwise = esDivisible (x-y) y