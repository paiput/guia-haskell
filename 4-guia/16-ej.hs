-- a) Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado
-- como par´ametro.
primerDivisor :: Integer -> Integer -> Integer
primerDivisor n m | mod n m == 0 = m
                  | otherwise = primerDivisor n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n = primerDivisor n 2

-- b) Implementar la funci´on esPrimo :: Integer ->Bool que indica si un n´umero natural pasado como par´ametro es primo
esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n

-- c) c) Implementar la funci´on sonCoprimos :: Integer ->Integer ->Bool que dados dos n´umeros naturales indica si no
-- tienen alg´un divisor en com´un mayor estricto que 1.
{-
  * Tengo que comparar cada divisor de n con cada divisor de m
    * Si algún divisor coincide, sonCoprimos es False
-}
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | n == m = False
                | 

esDivisor :: Integer -> Integer -> Bool
esDivisor n k = mod n k == 0

-- aux debería arrancar en 2 que es el primer num con el que empiezo a comparar buscando divisores
cantDivisores :: Integer -> Integer -> Integer
cantDivisores n aux | n == aux = 1
                    | esDivisor n aux  = 1 + cantDivisores n (aux+1)
                    | otherwise = cantDivisores n (aux+1)

-- comparteDivisores :: Integer -> Integer -> Bool
-- comparteDivisores n aux 

-- iEsimoDivisor :: Integer -> Integer -> Integer
-- iEsimoDivisor n i | 