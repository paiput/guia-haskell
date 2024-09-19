-- 1. sumaTotalProgresiva :: (Num t) => [t] -> [t] seg´un la siguiente especificaci´on:
-- problema sumaTotalProgresiva (s: seq⟨T⟩) : seq⟨T⟩ {
-- requiere: {T es un tipo num´erico}
-- requiere: {cada elemento de s es mayor estricto que cero}
-- asegura: {|s| = |resultado| ∧ el valor en la posici´on i de resultado es Pi
-- k=0 s[k]}
-- }
-- Por ejemplo sumaTotalProgresiva [1, 2, 3, 4, 5] es [1, 3, 6, 10, 15].
-- P´agina 3 de 5 Compilado el 2024/09/08 a las 00:16:08

sumatoria :: (Num t) => [t] -> t
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

invertir :: (Num t) => [t] -> [t]
invertir [] = []
invertir (x:xs) = (invertir xs) ++ [x]

sumaTotalProgresiva :: (Num t) => [t] -> [t]
sumaTotalProgresiva [x] = [x]
sumaTotalProgresiva (x:xs) = (sumatoria (x:xs)):(sumaTotalProgresiva xs)

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada xs = invertir (sumaTotalProgresiva (invertir xs))

-- 2. descomponerEnPrimos :: [Integer] -> [[Integer]] seg´un la siguiente especificaci´on:
-- problema descomponerEnPrimos (s: seq⟨Z⟩) : seq⟨seq⟨Z⟩⟩ {
-- requiere: { Todos los elementos de s son mayores a 2 }
  -- asegura: { |resultado| = |s| }
  -- asegura: {todos los valores en las listas de resultado son n´umeros primos}
  -- asegura: {
  --  multiplicar todos los elementos en la lista en la posici´on i de resultado es igual al valor 
  --  en la posici´on i de s
  -- }
-- }
-- Por ejemplo descomponerEnPrimos [2, 10, 6] es [[2], [2, 5], [2, 3]]
primerDivisor :: Integer -> Integer -> Integer
primerDivisor n m | mod n m == 0 = m
                  | otherwise = primerDivisor n (m+1)

menorDivisor :: Integer -> Integer
menorDivisor n = primerDivisor n 2

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n

primosHastaN :: Integer -> [Integer]
primosHastaN 0 = []
primosHastaN 2 = [2]
primosHastaN n = 

descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = [[]]
descomponerEnPrimos 