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