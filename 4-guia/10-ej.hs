-- Ejercicio 10. Especificar, implementar y dar el tipo 
-- de las siguientes funciones (s´ımil Ejercicio 4 Pr´actica 2 de Algebra 1)

-- a)
f1 :: Integer -> Integer
f1 0 = 1
f1 n = 2^n + f1 (n-1)





-- b)
--               Float                          VER ESTO DE Integer -> Float -> Integer
f2 :: Integer -> Integer -> Integer
f2 1 q = q
f2 n q = q^n + f2 (n-1) q




-- c)
sumatoria :: Integer -> Integer -> Integer
sumatoria 0 _ = 0
sumatoria n q = q^n + sumatoria (n-1) q

f3 :: Integer -> Integer -> Integer
f3 n q = sumatoria (n*2) q

-- yo (mal)
-- f3 1 q = q
-- f3 n q = q^n + f3 ((n*2)-1) q

-- Chatgpt
-- f3 p q
--     | q == 1    = 2 * p  -- caso especial cuando q = 1
--     | otherwise = (q * (q ^ (2 * p) - 1)) `div` (q - 1)




-- d) lo mismo que el c pero el indice aarranca en i=n
f4 :: Integer -> Integer -> Integer
f4 n q = q^n + sumatoria (n*2) q 

