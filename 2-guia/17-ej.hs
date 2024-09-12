fibonacciMayor :: Integer -> Integer -> Integer
fibonacciMayor n m | fibonacci m >= n = m
                   | otherwise = fibonacciMayor n (m+1)

esFibonacci :: Integer -> Bool
esFibonacci 1 = True
esFibonacci n = n == fibonacci (fibonacciMayor n 1)