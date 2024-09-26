# Simulacro Haskell - Tema 2

## Enunciado

### Ejercicio 1
Para empezar a diseñar la novedosa y rupturista red social Y el famoso Elio Mark nos ha pedido que desarrollemos algunas funciones básicas, que tendrán como objetido representar algunas relaciones e interacciones entre los usuarios. Para esto nos envió las siguientes especificaciones en lenguaje semiformal y nos pidió que hagamos el desarrollo enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que se ven en Introducción a la Programación de Exactas-UBA.

```
problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
  requiere: {True}
    asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
  }
```
1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.

```
problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res no tiene elementos repetidos}
  asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
}
```

```
problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
}
```

```
problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
  requiere: {relaciones no vacía}
  requiere: {relacionesValidas(relaciones)}
  asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
}
```
    
### Ejercicio 2

Conteste marcando la opción correcta. En el contexto de la programación funcional, se llama polimorfismo:

  [x] Cuando una función puede invocarse con distintos tipos de datos sin tener que redefinirla.
  [ ] Cuando una función puede invocarse con distintos tipos de datos teniendo que definirla para cada tipo de dato particular.
  [ ] Cuando tengo un tipo de dato que puede ser usado para invocar a muchas funciones.
