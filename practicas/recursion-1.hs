parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = 1 + parteEntera (n - 1)
--parteEntera 0 = 0
--parteEntera (-1) = 0
--parteEntera n = i + parteEntera (n-1) where i = 0






iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = (div n (10 ^ (cantDigitos n - i))) `mod` 10

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos(div n 10)

-- solucion profes (recursiva)
iesimoDigito2 :: Integer -> Integer -> Integer
iesimoDigito2 n 1 = div n (10^(cantDigitos n - 1))
iesimoDigito2 n i = iesimoDigito2 (mod n (10 ^ cantDigitos (n-1))) (i-1)






esCapicua :: Integer -> Bool
esCapicua n | cantDigitos n == 1 = True
            | otherwise = iesimoDigito n 1 == iesimoDigito n (cantDigitos n) && esCapicua (div (mod n (10^((cantDigitos n) - 1))) 10)

-- soluciones profes
esCapicua3 :: Integer -> Bool
esCapicua3 n | 0 <= n && n < 10 = True
             | 10 <= n && n < 100 = (iesimoDigito n 1) == (iesimoDigito n 2)
             | otherwise = primero == ultimo && esCapicua(sacarPrimeroYUltimo n)
             where primero = iesimoDigito n 1
                   ultimo = mod n 10

-- soluciones profes esCapicua3
sacarPrimeroYUltimo :: Integer -> Integer
sacarPrimeroYUltimo n = div (mod n (10^((cantDigitos n) - 1))) 10