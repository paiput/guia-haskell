-- 1. sumatoria :: [Integer] -> Integer seg´un la siguiente especificaci´on:
-- problema sumatoria (s: seq⟨Z⟩) : Z {
-- requiere: { T rue }
-- asegura: { resultado =
-- P|s|−1
-- i=0 s[i] }
-- }
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- 4. sumarN :: Integer -> [Integer] -> [Integer] seg´un la siguiente especificaci´on:
-- problema sumarN (n: Z, s: seq⟨Z⟩) : seq⟨Z⟩ {
-- requiere: { T rue }
-- asegura: {|resultado| = |s| ∧ cada posici´on de resultado contiene el valor que hay en esa posici´on en s sumado
-- n }
-- }
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (n + x):(sumarN n xs)

-- multiplosDeN :: Integer -> [Integer] -> [Integer] que dado un n´umero n y una lista xs, devuelve una lista
-- con los elementos de xs m´ultiplos de n.
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)
  | mod x n == 0 = x:(multiplosDeN n xs)
  | otherwise = multiplosDeN n xs