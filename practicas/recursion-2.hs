sumaPotenciaHastaN :: Integer -> Integer -> Integer
sumaPotenciaHastaN q 1 = q
sumaPotenciaHastaN q n = q^n + sumaPotenciaHastaN q (n-1)


{-
problema sumaPotencias(q:Z, n:Z, m:Z): Z {
    requiere: { q > 0 y n > 0 y m > 0}
    asegura: { res es (sumatoria de a=1 hasta n sumatoria de b=1 hasta m): q^(a+b) }
}
-}

potenciaRecursiva :: Integer -> Integer -> Integer -> Integer
potenciaRecursiva q 1 m = q^(m+1)
potenciaRecursiva q n m = q^(n+m) + potenciaRecursiva q (n-1) m

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n 1 = potenciaRecursiva q n 1
sumaPotencias q n m = sumaPotencias q n (m-1) + potenciaRecursiva q n m 
--sumaPotencias q n m = potenciaRecursiva q n m + potenciaRecursiva q m n 




-- menorDivisor
{-
problema menorDivisor() {
    requiere: {}
    asegura: {}
}
-}

primerDivisor :: Integer -> Integer -> Integer
primerDivisor n m | mod n m == 0 = m
                  | otherwise = primerDivisor n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n = primerDivisor n 2



esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n





-- ej 19
-- n es el indice del n-esimo primo, y k es el n-esimo primo
-- k arranca en 2 siempre
nEsimoPrmo :: Integer -> Integer -> Integer
nEsimoPrmo n k | n == 0 = k - 1                         -- el -1 es porque en la última vuelta estoy sumando 1 igual
               | esPrimo k = nEsimoPrmo (n-1) (k+1)     
               | not (esPrimo k) = nEsimoPrmo n (k+1) 

-- suma los primeros i primos
sumaNPrimos :: Integer -> Integer
sumaNPrimos 1 = 2
sumaNPrimos n = nEsimoPrmo n 2 + sumaNPrimos (n-1)

esSumaDeNPrimos :: Integer -> Integer -> Bool
esSumaDeNPrimos n m | n == sumaNPrimos m = True
                    | n < sumaNPrimos m = False
                    | otherwise = esSumaDeNPrimos n (m+1)

-- esSumaDeNPrimos n 1 -- va 1 como parametro m ya que es el indice del primer primo (arrancamos con 1), que es 2

esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = esSumaDeNPrimos n 1



-- solucion profes 
-- esSumaInicialDePrimos2 :: Int -> Bool
-- esSumaInicialDePrimos2 n = esSumaDePrimerosMPrimos 1 n

-- esSumaDePrimerosMPrimos :: Int -> Int -> Bool
-- esSumaDePrimerosMPrimos m n
--     | esSumaDePrimerosMPrimos m n == n = True
--     | esSumaDePrimerosMPrimos m n > n = False
--     | otherwise = esSumaDePrimerosMPrimos (m+1) n

-- ... no se termino por la hora