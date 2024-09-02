{-
Ejercicio 3. Implementar una funci´on estanRelacionados :: Integer ->Integer ->Bool
problema estanRelacionados (a:Z, b:Z) : Bool {
  requiere: {a ̸= 0 ∧ b ̸= 0}
  asegura: {(res = true) ↔ a ∗ a + a ∗ b ∗ k = 0 para alg´un k ∈ Z con k ̸= 0)}
}
Por ejemplo:
estanRelacionados 8 2 ⇝ True porque existe un k = −4 tal que 82 + 8 × 2 × (−4) = 0. 
estanRelacionados 7 3 ⇝ False porque no existe un k entero tal que 72 + 7 × 3 × k = 0.
-}

-- a ∗ a + a ∗ b ∗ k = 0 
-- a * a = - a * b * k
-- - a = b * k
-- - a / b = k

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados x y = rem x y == 0

