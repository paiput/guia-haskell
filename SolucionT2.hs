module SolucionT2 where

-- Auxiliar
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- Auxiliar
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
    | pertenece x xs = eliminarRepetidos xs
    | otherwise = x:eliminarRepetidos xs

-- Auxiliar
contarApariciones :: (Eq t) => t -> [t] -> Int
contarApariciones _ [] = 0
contarApariciones n (x:xs)
    | n == x = 1 + contarApariciones n xs
    | otherwise = contarApariciones n xs

-- Auxiliar
ordenar :: (Ord t) => [t] -> [t]
ordenar [] = []
ordenar xs = (ordenar (quitar (maximo xs) xs)) ++ [maximo xs]
    where 
        maximo :: (Ord t) => [t] -> t
        maximo [x] = x
        maximo (x:y:xs)
            | x > y = maximo (x:xs)
            | otherwise = maximo (y:xs)
        quitar :: (Eq t) => t -> [t] -> [t]
        quitar _ [] = []
        quitar n (x:xs) 
            | n == x = xs
            | otherwise = x:(quitar n xs)

-- Auxiliar
masRepetido :: (Eq t) => [t] -> t
masRepetido [x] = x
masRepetido [x,y] = x
masRepetido lista = head (masRepetidoAux (eliminarRepetidos lista) lista)

-- Auxiliar
masRepetidoAux :: (Eq t) => [t] -> [t] -> [t]
masRepetidoAux [x] _ = [x]
masRepetidoAux (x:y:lsBase) lsRep
    | contarApariciones x lsRep > contarApariciones y lsRep = x:masRepetidoAux lsBase lsRep
    | otherwise = y:masRepetidoAux lsBase lsRep
    
-- Auxiliar personas
personasRepetidas :: [(String, String)] -> [String]
personasRepetidas [] = []
personasRepetidas ((p1, p2):ps) = ([p1]++[p2])++personasRepetidas ps

-- 1. problema relacionesValidas
relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas [(p1, p2)] = p1 /= p2
relacionesValidas (relacion:relaciones) = esValida relacion && relacionesValidas relaciones
    where esValida (p1, p2) = p1 /= p2 && not (pertenece (p1, p2) relaciones) && not (pertenece (p2, p1) relaciones)

-- 2. problema personas 
personas :: [(String, String)] -> [String]
personas [] = []
personas relaciones = eliminarRepetidos (personasRepetidas relaciones)

-- 3. problema amigosDe
amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe persona ((p1, p2):personas)
    | persona == p1 = p2:amigosDe persona personas
    | persona == p2 = p1:amigosDe persona personas
    | otherwise = amigosDe persona personas

-- 4. problema personaConMasAmigos
personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [(p1, p2)] = p1
personaConMasAmigos relaciones = masRepetido (personasRepetidas relaciones)

{-
lista para ir testeando
[("a","b"),("c","d"),("a","d")]
-}

-- COMPROBANTE
-- dG_xOibY 