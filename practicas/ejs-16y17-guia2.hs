{-
Implementar menorDivisor :: Integer ->Integer que calcule el menor divisor (mayor que 1) de un natural n pasado
como par´ametro.
-}

primerDivisor :: Integer -> Integer -> Integer
primerDivisor n m | mod n m == 0 = m
                  | otherwise = primerDivisor n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n = primerDivisor n 2


{-
implementar la funci´on esFibonacci :: Integer ->Bool seg´un la siguiente especificaci´on:
problema esFibonacci (n: Z) : B {
    requiere: { n ≥ 0 }
    asegura: { resultado = true ↔ n es alg´un valor de la secuencia de Fibonacci definida en el ejercicio 1}
}
-}
fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- devuelve el indice del primer fibonacci mayor o igual que n 
fibonacciMayor :: Integer -> Integer -> Integer
fibonacciMayor n m | fibonacci m >= n = m
                   | otherwise = fibonacciMayor n (m+1)

esFibonacci :: Integer -> Bool
esFibonacci 1 = True
esFibonacci n = n == fibonacci (fibonacciMayor n 1)


{-menorDivisor :: Integer -> Integer
menorDivisor n | n `mod` (i) == 0     = i
               | n `mod` (i+1) == 0 = i + 1
               where i = 1-}