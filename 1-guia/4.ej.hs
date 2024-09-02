{-
Ejercicio 4. ⋆
Especificar e implementar las siguientes funciones utilizando tuplas para representar pares, ternas de n´umeros.
a) prodInt: calcula el producto interno entre dos tuplas R × R.
b) todoMenor: dadas dos tuplas R×R, decide si es cierto que cada coordenada de la primera tupla es menor a la coordenada
correspondiente de la segunda tupla.
c) distanciaPuntos: calcula la distancia entre dos puntos de R
2
.
d) sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
e) sumarSoloMultiplos: dada una terna de n´umeros enteros y un natural, calcula la suma de los elementos de la terna que
son m´ultiplos del n´umero natural. Por ejemplo:
sumarSoloMultiplos (10,-8,-5) 2 ⇝ 2
sumarSoloMultiplos (66,21,4) 5 ⇝ 0
sumarSoloMultiplos (-30,2,12) 3 ⇝ -18
f) posPrimerPar: dada una terna de enteros, devuelve la posici´on del primer n´umero par si es que hay alguno, y devuelve
4 si son todos impares.
g) crearPar :: a ->b ->(a, b): crea un par a partir de sus dos componentes dadas por separado (debe funcionar para
elementos de cualquier tipo).
h) invertir :: (a, b) ->(b, a): invierte los elementos del par pasado como par´ametro (debe funcionar para elementos
de cualquier tipo).
i) Reescribir los ejercicios prodInt, todoMenor y distanciaPuntos usando el siguiente renombre de tipos: type Punto2D
= (Float, Float)
-}