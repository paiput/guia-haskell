quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) 
  | n == x = xs
  | otherwise = x:(quitar n xs)

-- quitarTodosLosN :: (Eq t) => t -> [t] -> [t]
-- quitarTodosLosN _ [] = []
-- quitarTodosLosN n (x:xs) 
--   | n == x = quitarTodosLosN n xs
--   | otherwise = x:(quitarTodosLosN n xs)

maximo :: (Ord t) => [t] -> t
maximo [x] = x
maximo (x:y:xs)
  | x > y = maximo (x:xs)
  | otherwise = maximo (y:xs)


{- MI INTENTO 
ordenar :: (Ord t) => [t] -> [t]
ordenar [x] = [x]
ordenar (x:y:xs)
  | x > y = y:(ordenar (x:xs))
  | otherwise = x:(ordenar (y:xs))

-- auxiliar para ordenar2
invertir :: (Ord t) => [t] -> [t]
invertir [] = []
invertir (x:xs) = (invertir xs) ++ [x]

ordenar2 :: (Ord t) => [t] -> [t]
ordenar2 [x] = [x]
ordenar2 xs = ordenar xs
FIN MI INTENTO-}

-- solucion profes
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar xs = (ordenar (quitar (maximo xs) xs)) ++ [maximo xs]



-- ej 6
type Texto = [Char]
type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

{-
FUNCIONES QUE HICE AL PEDOOOO
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

largoNombre :: Nombre -> Int
largoNombre [] = 0
largoNombre (iLetra:nombre) = 1 + largoNombre nombre 

nombresIguales :: Nombre -> Nombre -> Bool
nombresIguales [] [] = True
nombresIguales n1 n2
  | largoNombre n1 /= largoNombre n2 = False
  | otherwise = head n1 == head n2 && nombresIguales (tail n1) (tail n2)
-}

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos nombre (contacto:contactos) = nombre == fst contacto || enLosContactos nombre contactos
-- como lo muestra la profe
-- enLosContactos nombre ((n,t):contactos) = nombre == n || enLosContactos nombre contactos




agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto (nuevoNom, nuevoTel) contactos
  | enLosContactos nuevoNom contactos = contactos
  | otherwise = contactos ++ [(nuevoNom, nuevoTel)]

-- defino otro pattern matching para quitar
quitarT :: (Eq t) => (t,t) -> [(t,t)] -> [(t,t)]
quitarT _ [] = []
quitarT (a,b) ((ax,bx):xs)
  | a == ax = xs
  | otherwise = (ax,bx):(quitarT (a,b) xs)


-- ver por que solo borra el primer contacto
-- ej: eliminarContacto ("Lucas","5") [("Jorge","1"),("Lucas","2"),("Pedro","53")]
-- eliminarContacto ("Pedro","5") [("Jorge","1"),("Lucas","5"),("Pedro","53")] --> Da [("Pedro","5"),("Pedro","5")]
eliminarContacto :: Contacto -> ContactosTel -> ContactosTel
eliminarContacto (nuevoNom, nuevoTel) contactos
  | enLosContactos nuevoNom contactos = quitarT (nuevoNom, nuevoTel) contactos
  | otherwise = contactos