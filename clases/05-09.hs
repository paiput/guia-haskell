-- factorial :: Int -> Int
-- factorial n
--   | n == 0 = 1
--   | n > 0 = n * factorial (n-1)

-- -- o tambien
-- factorial n 
--   | n == 0 = 1
--   | otherwise = n * factorial (n-1)

-- -- o tambien con pattern matching
-- factorial 0 = 1
-- factorial n = n * factorial (n-1)


-- esPar :: Int -> Bool
-- esPar n 
--   | n == 0 = True
--   | n == 1 = False
--   | otherwise = esPar (n - 2)

-- -- otra forma
-- esPar n
--   | n == 0 = True
--   | not (esPar (n - 1))

-- sumaLosPrimerosNImpares :: Integer -> Integer
-- sumaLosPrimerosNImpares n
--   | n == 1 = 1
--   | n > 1 = nEsimoImpar + sumaLosPrimerosNImpares (n - 1)
--   where nEsimoImpar = 2 * n - 1



sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n 1 = 1
sumaDivisoresHasta n k
  | mod n k == 0 = sumaDivisoresHasta n (k - 1) + k
  | otherwise = sumaDivisoresHasta n (k - 1)

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n



-- ver si se puede hacer
sumaDivisores2 :: Integer -> Integer
sumaDivisores2 1 = 1 
sumaDivisores2 n 
  | mod n k == 0 = sumaDivisores2 (k - 1) + k
  | otherwise = sumaDivisores2 (k - 1)
  where k = n



-- hace n recursiones
sumatoriaInterna :: Integer -> Integer -> Integer
sumatoriaInterna n 0 = 0
sumatoriaInterna n m = (n^m) + sumatoriaInterna n (m - 1)

-- hace n^m recursioens
sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble 0 m = 0
sumatoriaDoble n m = sumatoriaInterna n m + sumatoriaDoble (n-1) m

-- ta mal
-- sumaDivisores :: Integer -> Integer
-- sumaDivisores 1 = 1
-- sumaDivisores
--   | mod n i == 0 = i
--   | otherwise = sumaDivisores (n - 1)
--   where i = 1






-- Ejercicio de la guía
ultimoDigito :: Integer -> Integer
ultimoDigito x = x `mod` 10

sacarUltimo :: Integer -> Integer
sacarUltimo x = x `div` 10

sumarDigitos :: Integer -> Integer
sumarDigitos x
  | x < 10 = x
  | otherwise = (ultimoDigito x) + sumarDigitos (sacarUltimo x)

-- forma todo junto:
sumarDigitos2 :: Integer -> Integer
sumarDigitos2 x
  | x < 10 = x
  | otherwise = (mod x 10) + sumarDigitos (div x 10)



-- otro ejercicio de la guía
-- m es el número contra el que comparo los digitos de n
compararTodos :: Integer -> Integer -> Bool
compararTodos n m
  | n < 10 = n == m
  | otherwise = (ultimoDigito n == m) && (compararTodos (sacarUltimo n) m)

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x = compararTodos x (ultimoDigito x)