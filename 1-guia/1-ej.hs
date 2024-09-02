-- Ejercicio 1. 

--a) Implentar la función parcial f :: Integer ->Integer definida por extensión de la siguiente manera:
-- f(1) = 8
-- f(4) = 131
-- f(16) = 16
-- cuya especificación es la siguiente:
-- problema f (n: Z) : Z {
--    requiere: {n = 1 ∨ n = 4 ∨ n = 16}
--    asegura: {(n = 1 → result = 8) ∧ (n = 4 → result = 131) ∧ (n = 16 → result = 16)}
-- }

f :: Integer -> Integer
f x | x == 1 = 8
    | x == 4 = 131
    | x == 16 = 16

-- b) Análogamente, especificar e implementar la función parcial g :: Integer ->Integer
-- g(8) = 16
-- g(16) = 4
-- g(131) = 1

g :: Integer -> Integer
g x | x == 8 = 16
    | x == 16 = 4
    | x == 131 = 1

-- c) A partir de las funciones definidas en los item 1 y 2, implementar las funciones parciales h = f ◦ g y k = g ◦ f
h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)