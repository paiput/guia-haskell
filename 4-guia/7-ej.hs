{-
Ejercicio 7. 

a) Implementar una funci´on:
distanciaManhattan:: (Float, Float, Float) ->(Float, Float, Float) ->Float

problema distanciaManhattan (p : R × R × R, q : R × R × R) : R {
    requiere: {T rue}
    asegura: {res = P2 i=0 |pi − qi|}
}

Por ejemplo:
distanciaManhattan (2, 3, 4) (7, 3, 8) ⇝ 9
distanciaManhattan ((-1), 0, (-8.5)) (3.3, 4, (-4)) ⇝ 12.8

b) Reimplementarla teniendo en cuenta el siguiente tipo: type Coordenada3d = (Float, Float, Float)
-}

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (p1, p2, p3) (q1, q2, q3) = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3)

type Coordenada3d = (Float, Float, Float)
distanciaManhattan2 :: Coordenada3d -> Coordenada3dn -> Float
distanciaManhattan2 (p1, p2, p3) (q1, q2, q3) = abs (p1 - q1) + abs (p2 - q2) + abs (p3 - q3)
