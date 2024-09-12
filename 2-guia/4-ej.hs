{-
Ejercicio 4. Especificar e implementar la funci´on sumaImpares :: Integer ->Integer que dado n ∈ N sume los primeros
n n´umeros impares. Por ejemplo: sumaImpares 3 ; 1+3+5 ⇝ 9.
-}
sumaImpares :: Integer -> Integer
sumaImpares 1 = 1
sumaImpares n = (2*i + 1) + sumaImpares(n-1) where i = n-1;