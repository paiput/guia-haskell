-- Un n´umero natural es perfecto cuando la suma de sus divisores propios (n´umeros que lo dividen menores a ´el) es igual
-- al mismo n´umero. Por ejemplo, 6 es un n´umero perfecto porque la suma de sus divisores propios (1,2 y 3) es igual a 6.

-- Dos n´umeros naturales distintos son amigos si cada uno de ellos se obtiene sumando los divisores propios del otro.
-- Por ejemplo, 220 y 284 son amigos porque los divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y 110 que
-- sumados dan 284 y los divisores propios de 284 son 1, 2 , 4, 71, 142 que sumados dan 220.

-- Ejercicio 9. Implementar la funci´on divisoresPropios :: Int ->[Int]
-- problema divisoresPropios (n: Z) : seq⟨Z⟩ {
--   requiere: {n > 0}
--   asegura: {res es la lista de divisores propios de n, ordenada de menor a mayor}
-- }

divisoresPropios :: Int -> [Int]
divisoresPropios 1 = [1]
divisoresPropios n = divisorPropioAux n 1
  where
    divisorPropioAux :: Int -> Int -> [Int]
    divisorPropioAux n x
      | n == x = []
      | x `esDivisorDe` n = x:divisorPropioAux n (x+1)
      | otherwise = divisorPropioAux n (x+1)

esDivisorDe :: Int -> Int -> Bool
esDivisorDe n x = mod x n == 0 


-- Ejercicio 10. Implementar la funci´on sonAmigos :: Int ->Int ->Bool
-- problema sonAmigos (n,m: Z) : Bool {
--   requiere: {n > 0}
--   requiere: {m > 0}
--   requiere: {m ̸= n}
--   asegura: {res = True ⇔ n y m son n´umeros amigos}
-- }
sonAmigos :: Int -> Int -> Bool
sonAmigos n m = sumarElementos (divisoresPropios n) == m || sumarElementos (divisoresPropios m) == n

sumarElementos :: [Int] -> Int
sumarElementos [] = 0
sumarElementos (x:xs) = x + sumarElementos xs


-- Ejercicio 11. Implementar la funci´on losPrimerosNPerfectos :: Int ->[Int]
-- problema losPrimerosNPerfectos (n: Z) : seq⟨Z⟩ {
  -- requiere: {n > 0}
  -- asegura: {res es la lista de los primeros n n´umeros perfectos, de menor a mayor}
-- }
-- Por cuestiones de tiempos de ejecuci´on, no les recomendamos que prueben este ejercicio con un n > 4.
losPrimerosNPerfectos :: Int -> [Int]
losPrimerosNPerfectos n = primerosNPerfectosAux n 1 0
  where
    primerosNPerfectosAux :: Int -> Int -> Int -> [Int]
    primerosNPerfectosAux base n encontrados
      | base <= encontrados = []
      | sumarElementos (divisoresPropios n) == n = n:primerosNPerfectosAux base (n+1) (encontrados+1)
      | otherwise = primerosNPerfectosAux base (n+1) encontrados


-- Ejercicio 12. Implementar la funci´on listaDeAmigos :: [Int] ->[(Int,Int)]
-- problema sonAmigos (lista: seq⟨Z⟩) : seq⟨Z × Z⟩ {
--   requiere: {Todos los n´umeros de lista son mayores a 0}
--   requiere: {Todos los n´umeros de lista son distintos}
--   asegura: {res es una lista de tuplas sin repetidos, que contienetuplas de dos n´umeros donde esos dos n´umeros
--   pertenecen a lista y son amigos}
--   asegura: {|res| es la cantidad de tuplas de dos n´umeros amigos que hay en lista. Consideraremos que la tupla (a, b)
--   (con a y b pertenecientes a Z) es igual a la tupla (b, a) para contar la cantidad de tuplas.}
-- }
listaDeAmigos :: [Int] -> [(Int, Int)]
listaDeAmigos [] = []
listaDeAmigos amigos = quitarTuplasRepetidas (juntarAmigos amigos amigos)

quitarTuplasRepetidas :: [(Int, Int)] -> [(Int, Int)]
quitarTuplasRepetidas [] = []
quitarTuplasRepetidas (tupla:lista)
  | tuplaPertenece tupla lista = quitarTuplasRepetidas lista
  | otherwise = tupla:quitarTuplasRepetidas lista

tuplaPertenece :: (Int,Int) -> [(Int,Int)] -> Bool
tuplaPertenece _ [] = False
tuplaPertenece (x,y) ((p,q):lista) = ((x == p && y == q) || (x == q && y == p)) || tuplaPertenece (x,y) lista
   
juntarAmigos :: [Int] -> [Int] -> [(Int, Int)]
juntarAmigos _ [] = []
juntarAmigos listaBase (x:xs) = juntarAmigosAux x (verAmigos x listaBase)++juntarAmigos listaBase xs
  where
    juntarAmigosAux :: Int -> [Int] -> [(Int, Int)]
    juntarAmigosAux _ [] = []
    juntarAmigosAux n (amigo:listaAmigos) = (n,amigo):juntarAmigosAux n listaAmigos

verAmigos :: Int -> [Int] -> [Int]
verAmigos _ [] = []
verAmigos n (x:xs)
  | sonAmigos n x = x:verAmigos n xs
  | otherwise = verAmigos n xs