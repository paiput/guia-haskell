identidad :: t -> t
identidad x = x

-- [1, 2, 3] :: [Int]
-- [div 10 5, div 4 2] :: [Int]
-- [[1], [2, 3], []] :: [[Int]]

-- operaciones del preludio de haskell
-- head :: [a] -> a
-- tail :: [a] -> [a]
-- (:) :: a -> [a] -> [a] -- agrega un elemento al final de la lista

-- head [1, 2, 3] : [4, 5] -- se puede
-- head ([1,2,3] : [4, 5]) : -- no se puede

-- [1..100] -- del 1 al 100
-- [1..] -- infinto
-- [1,3..100] -- va saltando de 2 en 2 

-- [1,0..(-100)] -- desde el 1 hasta -100
-- [(-19),(-15)..20] -- congruentes de mod 4 de -20 a 20

longitud :: [t] -> Int
longitud [] = 0
-- longitud (_:xs) = 1 + longitud xs 
longitud xs = 1 + longitud (tail xs)

sumatoria :: [Int] -> Int
sumatoria [] = 0
-- sumatoria (x:xs) = x + sumatoria xs
sumatoria xs = head xs + sumatoria (tail xs)

pertenece :: (Eq t) => [t] -> t -> Bool
pertenece [] _ = False
-- pertenece (x:xs) y
--   | x == y = True
--   | otherwise = pertenece xs y
pertenece xs y = head xs == y || pertenece (tail xs) y

-- (x:[]) es lo mismo que [x]

-- 1:(2:(3:[])) = [1,2,3]

losPrimerosSonIguales :: (Eq t) => [t] -> Bool
-- losPrimerosSonIguales [] = False
-- losPrimerosSonIguales [_] = False
losPrimerosSonIguales (x:y:xs) = x == y
losPrimerosSonIguales _ = False -- asi es mejor que en dos lineas (es un "en cualquier otro caso")


sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:str) 
  | x == ' ' && y == ' ' = sacarBlancosRepetidos (y:str)
  | otherwise = x:(sacarBlancosRepetidos (y:str))


-- 
sacarBlancos :: [Char] -> [Char]
sacarBlancos [] = []
sacarBlancos [x] = [x]
sacarBlancos [x, ' '] = [x]
sacarBlancos (c1:c2:str) 
  | c1 == ' ' = sacarBlancos (c2:str)
  | otherwise = c1:(sacarBlancos (c2:str))


contarPalabras :: [Char] -> Int
contarPalabras [] = 0
contarPalabras [x] 
  | x == ' ' = 0
  | otherwise = 1
contarPalabras (x:y:xs) 
  | x /= ' ' && y == ' ' = 1 + contarPalabras xs
  | otherwise = contarPalabras (y:xs)


-- requiere: no empieza con espacios
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras p = (primeraPalabra p):(palabras (sacarPrimeraPalabra p))

-- requiere: no empieza con espacios
primeraPalabra :: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra (x:xs)
  | x == ' ' = []
  | otherwise = x:(primeraPalabra xs)

-- requiere: no empieza con espacios
sacarPrimeraPalabra :: [Char] -> [Char]
sacarPrimeraPalabra [x] = []
sacarPrimeraPalabra (x:y:xs) 
  | x == ' ' && y /= ' ' = y:xs
  | otherwise = sacarPrimeraPalabra (y:xs)




palabraMasLarga :: [Char] -> [Char]
palabraMasLarga xs = palabraMasLargaAux (palabras xs)

palabraMasLargaAux :: [[Char]] -> [Char]
palabraMasLargaAux [p] = p
palabraMasLargaAux (x:y:xs)
  | longitud x >= longitud y = palabraMasLargaAux (x:xs)
  | otherwise = palabraMasLargaAux (y:xs)



aplanar :: [[Char]] -> [Char]
aplanar [] = []
-- aplanar [x] = x
aplanar (x:xs) = x ++ (aplanar xs)
