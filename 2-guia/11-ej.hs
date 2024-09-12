-- a) Especificar e implementar una funci´on eAprox :: Integer ->Float que aproxime el valor del n´umero e
-- a partir de la siguiente sumatoria:

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

e :: Integer -> Float
e n | n == 0    = 1
    | otherwise = (1 / fromIntegral (factorial n) + e (n-1))