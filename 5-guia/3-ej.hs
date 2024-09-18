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