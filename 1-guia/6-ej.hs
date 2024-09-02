{-
Ejercicio 6. Usando los siguientes tipos:
type Anio = Integer
type EsBisiesto = Bool

Programar una funci´on bisiesto :: Anio ->EsBisiesto seg´un la siguiente especificaci´on:

problema bisiesto (a˜no: Z) : Bool {
  requiere: {T rue}
  asegura: {res = f alse ↔ a˜no no es m´ultiplo de 4, o a˜no es m´ultiplo de 100 pero no de 400}
}

Por ejemplo:
bisiesto 1901 ⇝ False, bisiesto 1904 ⇝ True,
bisiesto 1900 ⇝ False, bisiesto 2000 ⇝ True.
-}

type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto n = not ((rem n 4 /= 0) || (rem n 100 == 0 && rem n 400 /= 0))

-- misma expresión sin el not
-- bisiesto n = (rem n 4 == 0) && (rem n 100 /= 0 || rem n 400 == 0)
