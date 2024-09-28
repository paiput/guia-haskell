-- Ejercicio 1. Implementar la funci´on productos :: [String] ->[(String, Int)]
-- problema generarStock (productos: seq⟨String⟩) : seq⟨String × Z⟩ {
--   requiere: {True}
--   asegura: { La longitud de res es igual a la cantidad de productos distintos que hay en productos}
--   asegura: {Para cada producto que pertenece a productos existe un i tal que 0 ≤ i < |res| y res[i]0=producto y
--   res[i]1 es igual a la cantidad de veces que aparece producto en productos}
-- }
type Producto = String
type Stock = (Producto, Int)
type Precio = (Producto, Float)

-- Aux
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) = x == n || pertenece n xs

-- Aux
contarAparicionesDe :: (Eq t) => t -> [t] -> Int
contarAparicionesDe _ [] = 0
contarAparicionesDe n (x:xs)
  | n == x = 1 + contarAparicionesDe n xs
  | otherwise = contarAparicionesDe n xs

-- Aux
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
  | pertenece x xs = eliminarRepetidos xs
  | otherwise = x:(eliminarRepetidos xs)

generarStock :: [Producto] -> [Stock]
generarStock productos = generarStockAux productos (eliminarRepetidos productos)
  where 
    generarStockAux :: [Producto] -> [Producto] -> [Stock]
    generarStockAux _ [] = []
    generarStockAux productosRepetidos (prod:productos) = (prod, (contarAparicionesDe prod productosRepetidos)):(generarStockAux productosRepetidos productos)

-- generarStock ["Banana", "Manzana", "Pera", "Manzana", "Pera", "Pera", "Frutilla"]


-- Ejercicio 2. Implementar la funci´on stockDeProducto :: [(String, Int))] ->String
-- problema stockDeProducto (stock: seq⟨String × Z⟩, producto: String ) : Z {
--   requiere: {No hay productos repetidos en stock}
--   requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
--   asegura: {(res = 0 y producto no se encuentra en el stock) o (existe un i tal que 0 ≤ i < |stock| y producto = stock[i]0
--   y res = stock[i]1}
-- }
stockDeProducto :: [Stock] -> Producto -> Int
stockDeProducto [] _ = 0
stockDeProducto ((prod, cant):stocks) producto 
  | producto == prod = cant
  | otherwise = stockDeProducto stocks producto
-- [("Banana",1),("Manzana",2),("Pera",3),("Frutilla",1)]



-- Ejercicio 3. Implementar la funci´on dineroEnStock :: [(String, Int))] ->[(String, Float)] ->Float
-- problema dineroEnStock (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : R {
--   requiere: {No hay productos repetidos en stock}
--   requiere: {No hay productos repetidos en precios}
--   requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
--   requiere: {Todas las precios (segundas componentes) de precios son mayores a cero}
--   requiere: {Todo producto de stock aparece en la lista de precios}
--   asegura: {res es igual a la suma de los precios de todos los productos que est´an en stock multiplicado por la cantidad
--   de cada producto que hay en stock}
-- }
-- Para resolver este ejercicio pueden utilizar la funci´on del Preludio de Haskell fromIntegral que dado un valor de tipo
-- Int devuelve su equivalente de tipo Float.
dineroEnStock :: [Stock] -> [Precio] -> Float
dineroEnStock [] _ = 0
dineroEnStock ((prod,cant):stocks) precios = fromIntegral cant * (encontrarPrecio prod precios) + dineroEnStock stocks precios
  where 
    encontrarPrecio :: Producto -> [Precio] -> Float
    encontrarPrecio _ [] = 0
    encontrarPrecio producto ((prod,precio):precios)
      | producto == prod = precio
      | otherwise = encontrarPrecio producto precios

-- [("Banana",1),("Manzana",2),("Pera",3),("Frutilla",1)]
-- [("Banana",4.5),("Manzana",7.5),("Pera",8.9),("Frutilla",10)]
-- [("Banana",1),("Manzana",1.5),("Pera",1.25),("Frutilla",10)]



-- Ejercicio 4. Implementar la funci´on aplicarOferta :: [(String, Int)] ->[(String, Float)] ->[(String,Float)]
-- problema aplicarOferta (stock: seq⟨String × Z⟩, precios: seq⟨String × R⟩ ) : seq⟨String × R⟩ {
--   requiere: {No hay productos repetidos en stock}
--   requiere: {No hay productos repetidos en precios}
--   requiere: {Todas las cantidades (segundas componentes) de stock son mayores a cero}
--   requiere: {Todas las precios (segundas componentes) de precios son mayores a cero}
--   requiere: {Todo producto de stock aparece en la lista de precios}
--   asegura: {|res| = |precios|}
--   asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) > 10, entonces res[i]0 = precios[i]0 y
--   res[i]1 = precios[i]1∗ 0,80}
--   asegura: {Para todo 0 ≤ i < |precios|, si stockDeProducto(stock, precios[i]0) ≤ 10, entonces res[i]0 = precios[i]0 y
--   res[i]1 = precios[i]1 }
-- }
aplicarOferta :: [Stock] -> [Precio] -> [Precio]
aplicarOferta _ [] = []
aplicarOferta stocks ((prod,precio):precios) = (prod, precio * calcularPorcentajeOferta (encontrarStock prod stocks)):aplicarOferta stocks precios
  where 
    encontrarStock :: Producto -> [Stock] -> Int
    encontrarStock _ [] = 0
    encontrarStock producto ((prod,stock):stocks)
      | producto == prod = stock
      | otherwise = encontrarStock producto stocks
    calcularPorcentajeOferta :: Int -> Float
    calcularPorcentajeOferta stock
      | stock > 10 = 0.8
      | otherwise = 1