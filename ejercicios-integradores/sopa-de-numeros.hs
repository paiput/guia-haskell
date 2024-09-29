type Fila     = [Int]
type Tablero  = [Fila]
type Posicion = (Int, Int)
type Camino   = [Posicion]

-- Ejercicio 5. Implementar la funci´on maximo :: Tablero ->Int
-- problema maximo (t: Tablero) : Z {
--   requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
--   menos un elemento}
--   requiere: {Existe al menos una columna en el tablero t }
--   requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
--   asegura: {res es igual al n´umero m´as grande del tablero t}
-- }
maximoFila :: Fila -> Int
maximoFila [x] = x
maximoFila (x:y:xs)
  | x > y = maximoFila (x:xs) 
  | otherwise = maximoFila (y:xs)

maximo :: Tablero -> Int
maximo [fila] = maximoFila fila
maximo (fila1:fila2:tablero)
  | maximoFila fila1 > maximoFila fila2 = maximo (fila1:tablero)
  | otherwise = maximo (fila2:tablero)  

{-
[[1,2,3],[3,4,5],[5,6,7]]
-}


-- Ejercicio 6. Implementar la funci´on masRepetido :: Tablero ->Int
-- problema masRepetido (t: Tablero) : Z {
--   requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
--   menos un elemento}
--   requiere: {Existe al menos una columna en el tablero t }
--   requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
--   asegura: {res es igual al n´umero que m´as veces aparece en un tablero t. Si hay empate devuelve cualquiera de ellos}
-- }
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) = n == x || pertenece n xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
  | pertenece x xs = eliminarRepetidos xs
  | otherwise = x:(eliminarRepetidos xs)

contarAparicionesDe :: (Eq t) => t -> [t] -> Int
contarAparicionesDe _ [] = 0
contarAparicionesDe n (x:xs)
  | n == x = 1 + contarAparicionesDe n xs
  | otherwise = contarAparicionesDe n xs

masRepetidoFila :: Fila -> Int
masRepetidoFila [x] = x
masRepetidoFila fila = masRepetidoAux fila (eliminarRepetidos fila)
  where
    masRepetidoAux :: Fila -> Fila -> Int
    masRepetidoAux _ [x] = x
    masRepetidoAux fila (x:y:filaSinRep)
      | contarAparicionesDe x fila > contarAparicionesDe y fila = masRepetidoAux fila (x:filaSinRep)
      | otherwise = masRepetidoAux fila (y:filaSinRep)

masRepetido :: Tablero -> Int
masRepetido [fila] = masRepetidoFila fila
masRepetido tablero = masRepetidoFila (unirFilas tablero)
  where
    unirFilas :: Tablero -> Fila
    unirFilas [fila] = fila
    unirFilas (fila:tablero) = fila++unirFilas tablero 
{-
[[1,2,3],[3,4,5],[5,6,7]]
[[1,1,1],[2,2,5],[2,2,7]]
-}

-- Ejercicio 7. Implementar la funci´on valoresDeCamino :: Tablero ->Camino ->[Int]
-- problema valoresDeCamino (t: Tablero, c: Camino) : seq⟨Z⟩ {
--   requiere: {El tablero t es un tablero bien formado, es decir, la longitud de todas las filas es la misma, y tienen al
--   menos un elemento}
--   requiere: {Existe al menos una columna en el tablero t }
--   requiere: {El tablero t no es vac´ıo, todos los n´umeros del tablero son positivos, mayor estricto a 0}
--   requiere: {El camino c es un camino v´alido, es decir, secuencia de posiciones adyacentes en la que solo es posible
--   desplazarse hacia la posici´on de la derecha o hacia abajo y todas las posiciones est´an dentro de los limites del tablero
--   t}
--   asegura: {res es igual a la secuencia de n´umeros que est´an en el camino c, ordenados de la misma forma que aparecen
--   las posiciones correspondientes en el camino.}
-- }
valoresDeCamino :: Tablero -> Camino -> [Int]
valoresDeCamino _ [] = []
valoresDeCamino tablero ((fil, col):camino) = (obtenerColumna col (obtenerFila fil tablero)):valoresDeCamino tablero camino
  where 
    obtenerFila :: Int -> Tablero -> Fila
    obtenerFila i tablero = iEsimoElemento i tablero
    obtenerColumna :: Int -> Fila -> Int 
    obtenerColumna i fila = iEsimoElemento i fila

largoDe :: (Eq t) => [t] -> Int
largoDe [] = 0
largoDe (x:xs) = 1 + largoDe xs

ultimoElementoDe :: (Eq t) => [t] -> t
ultimoElementoDe [x] = x
ultimoElementoDe (x:xs) = ultimoElementoDe xs

quitarUltimo :: (Eq t) => [t] -> [t]
quitarUltimo [x] = []
quitarUltimo (x:xs) = x:(quitarUltimo xs)

iEsimoElemento :: (Eq t) => Int -> [t] -> t
iEsimoElemento i lista
  | largoDe lista == i = ultimoElementoDe lista
  | otherwise = iEsimoElemento i (quitarUltimo lista)

{-
[
  [1,3,5],
  [3,9,4],
  [6,2,6]
]

[(1,2),(1,3),(2,3)]
-}


-- Ejercicio 8. Implementar la funci´on esCaminoFibo :: [Int] ->Int ->Bool
-- problema esCaminoFibo (s:seq⟨Z⟩, i : Z) : Bool {
--   requiere: {La secuencia de n´umeros s es no vac´ıa y est´a compuesta por n´umeros positivos (mayor estricto que 0)
--   que representan los valores de un camino en un tablero}
--   requiere: {i ≥ 0}
--   asegura: {res = true ⇔ los valores de s son la sucesi´on de Fibonacci inicializada con el n´umero pasado como
--   par´ametro i}
-- }
-- Notas: En este ejercicio se pasa una secuencia de valores en lugar de un tablero y un camino para no generar dependencia
-- con el ejercicio anterior. Recordemos que la sucesi´on de Fibonacci est´a definida con la siguiente funci´on recursiva:
-- f(0) = 0
-- f(1) = 1
-- f(n) = f(n-1) + f(n-2) con n>1
-- En el ejemplo del tablero y del camino (verde claro) que figuran m´as arriba tenemos que esCaminoFibo [1,1,2,3,5] 1
-- reduce a True.
esCaminoFibo :: [Int] -> Int -> Bool
esCaminoFibo [x] iFibo = esFiboAux iFibo x
esCaminoFibo (x:xs) primerIFibo = esFiboAux primerIFibo x && esCaminoFibo xs primerIFibo

-- esNumeroFibonacci :: Int -> Bool
-- esNumeroFibonacci n = esFiboAux 0 n
--   where
esFiboAux :: Int -> Int -> Bool
esFiboAux iFibo n
  | n == fibonacci iFibo = True
  | n < fibonacci iFibo = False
  | otherwise = esFiboAux (iFibo+1) n

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2) 

-- esCaminoFibo [1,2,3,5] 2 // True