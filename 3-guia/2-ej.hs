-- Ejercicio 2. ⋆ Especificar e implementar las siguientes funciones, incluyendo su signatura.

-- a) absoluto: calcula el valor absoluto de un n´umero entero.
{-
problema absoluto(x: Z): Z {
  requiere: {True}
  asegura: {res es el modulo de x}
}
-}
absoluto :: Integer -> Integer
absoluto x | x < 0 = -x
           | otherwise = x


-- b) maximoabsoluto: devuelve el m´aximo entre el valor absoluto de dos n´umeros enteros.
{-
problema maximoAbsoulto(x: Z, y: Z): Z {
  requiere: {True}
  asegura: {res es el mayor de entre el modulo de x y el modulo de y}
}
-}
maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto x y
  | absoluto x > absoluto y = absoluto x
  | otherwise = absoluto y


-- c) maximo3: devuelve el m´aximo entre tres n´umeros enteros.
{-
problema maximo3(x: Z, y: Z, z: Z): Z {
  requiere: {True}
  asegura: {res es el de entre x, y, z}
}
-}
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z
  | x > y && x > z = x
  | y > z = y
  | otherwise = z


-- d) algunoEs0: dados dos n´umeros racionales, decide si alguno de los dos es igual a 0 (hacerlo dos veces, una usando pattern
-- matching y otra no).
{-
problema algunoEs0(x: Q, y: Q): Bool {
  requiere: {True}
  asegura: {res es True si x = 0 ∨ y = 0, sino es False}
}
-}
algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y
  | x == 0 || y == 0 = True
  | otherwise = False

algunoEs02 x y
  | x == 0 || y == 0 = True
  | otherwise = False


-- e) ambosSon0: dados dos n´umeros racionales, decide si ambos son iguales a 0 (hacerlo dos veces, una usando pattern matching
-- y otra no).
{-
problema ambosSon0(x: Q, y: Q): Bool {
  requiere: {True}
  asegura: {res es True si x = 0 ∧ y = 0, sino es False}
}
-}
ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = x == 0 && y == 0

ambosSon02 x y = x == 0 && y == 0


-- f) mismoIntervalo: dados dos n´umeros reales, indica si est´an relacionados considerando la relaci´on de equivalencia en R
-- cuyas clases de equivalencia son: (−∞, 3],(3, 7] y (7, ∞), o dicho de otra forma, si pertenecen al mismo intervalo.
{-
I = {(−∞, 3], (3, 7], (7, ∞)}
problema mismoIntervalo(x: R, y: R): Bool {
  requiere: {True}
  asegura: {res es True si x, y ∈ ambas a alguno de los intervalos I}
}
-}
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y
  | x <= 3 && y <= 3 = True
  | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
  | x > 7 && y > 7 = True
  | otherwise = False


-- g) sumaDistintos: que dados tres n´umeros enteros calcule la suma sin sumar repetidos (si los hubiera).
{-
problema sumaDistintos(x: Z, y: Z, z: Z): Z {
  requiere: {True}
  asegura: {res es la suma de todos los valores distintos entre se de x, y, z}
}
-}
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z
  | x == y && y == z = x
  | x == y || y == z = x + z
  | x == z = x + y
  | otherwise = x + y + z


-- h) esMultiploDe: dados dos n´umeros naturales, decidir si el primero es m´ultiplo del segundo.
{-
problema esMultiploDe(x: Z, y: Z): Bool {
  requiere: {True}
  asegura: {res es True si x / y tiene resto 0}
}
-}
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = div x y * y == x


-- i) digitoUnidades: dado un n´umero entero, extrae su d´ıgito de las unidades.
{-
problema digitoUnidades(x: Z): Z {
  requiere: {True}
  asegura: {res es el ultimo digito de x}
}
-}
digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10


-- j) digitoDecenas: dado un n´umero entero mayor a 9, extrae su d´ıgito de las decenas.
{-
problema digitoDecenas(x: Z): Z {
  requiere: {x > 9}
  asegura: {res es el digito de las decenas de x}
}
-}
digitoDecenas :: Integer -> Integer
digitoDecenas x = div (mod x 100) 10