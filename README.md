## ¿Paradigma Funcional? ¿Qué es eso?

Es irónico que aunque el paradigma funcional es muy anterior al paradigma de objetos, lo que le ha dado la posibilidad de construir sólidas bases, es difícil dar una definición del mismo. Por ejemplo, la definición más obvia reza
funcional es un paradigma en el que las soluciones a los problemas se estructuran en términos de aplicación de funciones
y si bien es correcta, hay tantos elementos fundamentales que se desprenden de ésta y que no son evidentes que resulta de poca utilidad.

Quizás sea más útil pensarlo a partir de las características más frecuentemente evocadas cuando se piensa en éste:

- Pureza: las funciones, al igual que en matemática, no presentan efectos colaterales, sino que tan sólo reciben, operan y devuelven valores
- Evaluación diferida: ciertas partes del código no se evaluarán salvo que sea necesario
- Funciones de primer orden: las funciones son valores, y por tanto pueden ser pasadas por parámetro
- Pattern matching: los valores pueden ser descompuestos estructuralmente, en un proceso inverso a la construcción: la deconstrucción. Y además podemos usar a esta herramienta como mecanismo de - control de flujo: según encaje un valor con un patrón u otro, podremos tomar acciones diferente.
- Expresiones lambda: Es posible escribir valores de función de forma literal, sin asignarle un nombre.
- Inmutabilidad: las variables son meras etiquetas, que una vez unificadas contra un valor, no pueden ser cambiadas

Sin embargo, ¿son las anteriores ideas propias del paradigma funcional? Miremos más en detalle:

- No todos los lenguajes funcionales son realmente puros. LISP y sus derivados, por ejemplo, no lo son normalmente: permiten hacer input-ouput (IO) imperativo, modificar variables, etc.
- No todos los lenguajes funcionales presentan evaluación diferida. Para ser justos, ni siquiera Haskell: éste ofrece evaluación no-estricta, lo cual es ligeramente diferente.


Por un lado muchos lenguajes (funcionales o no) presentan algún tipo de operación de deconstrucción: Ruby, ECMAScript6, Clojure, etc, que es la base para implementar pattern-matching. Y por otro lado, la idea de Pattern matching, no figura en Calculo Lambda, la base teórica de funcional.

Virtualmente todos los lenguajes modernos presentan lambdas, closures o bloques de código, que permiten cosificar una porción de código.

Si nada de lo que parece tan propio de funcional es realmente exclusivo del mismo, entonces, volvemos a la pregunta: ¿qué es eso? Simple: es la forma particular en que combinamos estas herramientas, razonando declarativamente en términos de valores y transformaciones sobre los mismos.
Nuevamente, el todo es más que la suma de las partes.


## Sobre pureza 

Además, es un lenguaje puro. Tanto, que hasta los efectos están modelados como valores de tipo IO, que representa un efecto, el cual puede ser operado como cualquier otro valor: podemos pasar efectos por parámetros, colocarlos en listas, ordenarlos, etc.
De hecho, un programa ejecutable es una función que devuelve un valor de tipo IO. El runtime de Haskell ejecuta el efecto representado por este valor, produciendo así los efectos en el mundo real deseados.
Moraleja: un programa Haskell no tiene efectos, pero es capaz de devolver un valor que los representa, pudiendo así hacer todo lo que un programa imperativo podría hacer, y más.
Simplicidad
La sintaxis e ideas fundamentales de Haskell son realmente simples, y el resto de las ideas más complejas se construyen normalmente sobre las más simples.

## Tipos en Haskell

A diferencia de otros lenguajes, como Ruby o Smalltalk, Haskell tiene un sistema de tipos estático. Esto significa que el tipo de cada expresión es conocida en tiempo de compilación. En los casos más simples si tenemos una función simple que suma dos números, y pasamos un string que se sume a un int, en un lenguaje dinámico solo fallaría en tiempo de ejecución, mientras en que en un lenguaje de sistema de tipos estático, no compilaría. Esto permite que puedan capturarse errores en tiempo de compilación en vez de que tengan que aparecer en ejecución. En Haskell todo tiene un tipo, y a diferencia de otros lenguajes similares como Java, Haskell tiene además inferencia de tipos. Si escribimos un número, no tenemos que decirle a Haskell que es un número, puede inferirlo solo, entonces no tenemos que escribir explícitamente que es un número al declararlo en una variable.

para ello mostramos que en Haskell se puede conocer el tipo de un valor por medio del comando :t

```haskell
Prelude> :t 'a'
'a' :: Char
Prelude> :t True
True :: Bool
```

como vemos en el GhCI, cuando ejecutamos el comando :t después de un valor, nos dice de que timo es, x :: T puede leerse como x es del tipo T.

con expresiones más complejas podemos ver algo como lo siguiente:

```haskell
Prelude> :t 4 + 3
4 + 3 :: Num a => a
```

Simple, todas las expresiones generan un valor con un tipo asociado.

Las funciones son otra cosa que también necesita una declaración de tipos, y es una buena práctica que ayuda al sistema de inferencia de tipos, y es recomendado a menos que se necesite crear funciones muy chicas. Empecemos con un ejemplo bien simple

por ej:

```haskell
Prelude> let succ a = a + 1
Prelude> :t succ
succ :: Num a => a -> a 
```
Esto se vería así en un código que no sea ghci

```haskell
succ :: Int -> Int
succ a = a + 1
```

Veamos un poco más en detalle otro prototipo de función, por ej. head y tail

```haskell
Prelude> :t head                                                                                                                                                                                     head :: [a] -> a                                                                                                                                                                                     Prelude> :t tail                                                                                                                                                                                     tail :: [a] -> [a]
```

Este ejemplo es bien conocido por tomar el primer elemento de una lista, head. Pero veamos la declaración de tipos, head toma un parámetro, que es del tipo lista de a. Pero a no es un tipo, que es entonces? En este caso es un tipo genérico, puede ser un Int, String, etc, pero el tipo es consistente, o sea si tenemos una funcion que va de [a] -> a, entonces si a es un Int, la función toma una lista de Int, y devuelve un Int. Esto es porque la función es polimórfica y puede tomar cualquier lista de un tipo y devolver el primer elemento, sin importar de que tipo es la lista, no tiene restricciones en ese sentido, sin embrargo nuestra función succ, solo toma un Int y devuelve otro Int, si le pasamos algo del tipo Strng fallaría en tiempo de compilación. En el ejemplo de head, en la declaración de tipos a, es llamado type variable.

tomemos otro ejemplo, fst

```haskell
Prelude> :t fst
fst :: (a, b) -> a
```

en este caso se puede ver como la función toma una tupla y devuelve el primer elemento, y tenemos dos type variables, a y b, que si bien son diferentes, no significa que sean de tipos distintos, lo que significa es que el primer elemento y lo que devuelve la función son del mismo tipo, tal como lo vimos con head.

## Typeclases

Una Typeclass es como una especie de interfaz que define un comportamiento, si un tipo es parte de un typeclass, el tipo soporta e implementa el comportamiento que describe el typeclass. Esto puede ser algo confuso para genete acostumbrada al paradigma de objetos, por lo que las typeclases son como las interfaces de Java, pero implementando el comportamiento, no solo definiendo su contrato.

veamos la operación suma del succ

```haskell
Prelude> :t (+)
(+) :: Num a => a -> a -> a
```

Antes que nada vemos que ahora esta el símbolo, =>, esto es un class constraint. La lectura hacia la derecha es como las funciones, toma dos elementos de tipo a y devuelve otro del tipo a, a la izquierda significa que el tipo de los dos valores y el retorno deben ser miembros de la clase Num, por eso se conoce a => como class constraint.

veamos otro ejemplo

```haskell
Prelude> :t (<=)
(<=) :: Ord a => a -> a -> Bool
```

Ord es otro typeclass que define la interfaz para ordenamiento, como <, >, <= y >=, por lo que cualquier tipo que requira o necesite ordenamiento de dos o más elementos, debe ser un mienbro de Ord., pero por ej, para ser miembro de Ord, un tipo tiene que ser miembro de Eq.

Volviendo a la suma, vimo que Num es un typeclass numerico, y permite que un tipo actue como números, por ej:

```haskell
Prelude> :t 42
42 :: Num a => a
```

por lo que los números pueden actuar como constantes polimorficas, por lo que podemos definir un 42 numérico, flotante o doble, pero hay operaciones que si bien son parte del tipeclass, su contrato debe ser cumplido, por ej. si sumamos un doble con un interfaces

```haskell
Prelude> (42 :: Integer) + (2 :: Double)

<interactive>:25:20:
    Couldn't match expected type ‘Integer’ with actual type ‘Double’
    In the second argument of ‘(+)’, namely ‘(2 :: Double)’
    In the expression: (42 :: Integer) + (2 :: Double)
    In an equation for ‘it’: it = (42 :: Integer) + (2 :: Double)
Prelude> (42 :: Integer) + (2 :: Integer)
44
```

## Dualidad en estructuras de tipos

cada una de ellas presenta una dualidad, pudiendo ser ser pensada tanto como una estructura de datos, como una estructura de control. Dicho de otra forma, a las estructuras funcionales podemos verlas tanto como contenedores (cajas que almacenan valores) como computaciones (operaciones que al ejecutarlas producen valores).

## Creando nuestros propios tipos 


Para crear tipos de estructura en Haskell lo hacemos mediante data <NOmbre del tipo de dato> = value constructors. Un ejemplo simple como el booleano sería:

```haskell
data Bool = False | True
```

tanto los valores como los tipos deben ser declarados en mayúscula.

Empecemos por un ejemplo bien sencillo, como es la lista

```haskell
data List a = Nil | Cons a (List a)
```

esto nos permite definir tanto una lista vacia como una lista con varios elementos

un ejemplo seria

```haskell
laLista123 = Cons 1 (Cons 2 (Cons 3 Nil))
```

pero podemos tambien escribirla de la siguiente manera

```haskell
laLista123 = [1, 2, 3]
```

veamos de crear algo un poco más complejo como declarar formas

data Forma = Circulo Float Float Float | Rectangulo Float Float Float Float

Que es lo que definimos aca?, veamos un poco, definimos un tipo de dat Forma, que puede ser tanto un círculo como un rectángulo, veamos un poco que interfaz exponen los tipos que acabamos de crear, Circulo y rectángulo:


```haskell
Prelude> data Forma = Circulo Float Float Float | Rectangulo Float Float Float Float
Prelude> :t Circulo
Circulo :: Float -> Float -> Float -> Forma
Prelude> :t Rectangulo
Rectangulo :: Float -> Float -> Float -> Float -> Forma
```

entonces los value constructors son como funciones como el resto de las cosas, algo un poco raro si venimos de objetos, armemos una funcion para calcular la superficie

```haskell
superficie :: Forma -> Float
superficie (Circulo _ _ r) = pi * r ^ 2
superficie (Rectangulo x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

Veamos ahora que si bien podemos tener una función que vaya de un Circulo a un Float (Circulo -> Float), en realidad no estaria bien, porque Circulo no es un tipo es una función, entonces por eso nuestra declaración de la función es de Forma -> Float, ahora dependiendo del constructor, por medio de patten matching, podemos calcular la superficie, sea un circulo o rectángulo.

Veamos de imprimir la forma de un Ciruclo en pantalla

```haskell
Prelude> Circulo 2 1 1

<interactive>:5:1:
    No instance for (Show Forma) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
```

Ouch!, no funciona asi, pero como hacemos para incluir en nuestro tipo un typeclass?

Usando algo llamamo deriving

```
Prelude> data Forma = Circulo Float Float Float | Rectangulo Float Float Float Float deriving (Show)
Prelude> Circulo 2 1 1
Circulo 2.0 1.0 1.0
```

Ahora, como los value constructors son funciones, podemos usarlos para aplicar parcialmente a estos por ej. un ejemplo es creando una lista de Ciruclos, en los que varian el último valor, pero no los dos primeros

```
Prelude> map (Circulo 100 200) [10, 23, 44]
[Circulo 100.0 200.0 10.0,Circulo 100.0 200.0 23.0,Circulo 100.0 200.0 44.0]
```

Veamos un ejemplo tal vez un poco más interesante, hmmm autos...

## Type parameters

### Maybe

Empecemos definiendo maybe

```haskell
data Maybe a = Just a | Nothing
```

¿Qué significa esto? Un Maybe representa algo que puede estar (Just), o no (Nothing). Es decir, si lo pensamos como un contenedor, Maybe es una caja que puede contener cero o un elemento.
Y si lo pensamos como como una computación, una función que devuelva Maybe representa una computación que puede o no arrojar un resultado, es decir, una computación que puede fallar.

Cual es la diferencia entre el Maybe y por ej, el booleano, las formas o la lista que definimos antes? El tipo de dato no define valores, sino nuevos tipos, a esto se lo conoce como type constructors, por lo que en este caso a puede ser de un tipo x, por lo que si tenemos un Maybe Int, puede derivar en un Nothing o un Just Int.

Por ejemplo el inverso, si la inversa de x es 1/x, entonces si x== 0, su inverso sería tendiendo a infinito. por lo que tenemos un caso en el que la computación puede fallar, porque no se puede computar a infinito, entonces veamos como podemos representar una función inversa usando Maybe.

```haskell
inverse 0 = Nothing
inverse x = Just (1 / x)
```

## Either

Veamos ahora una estructura que pueda tomar dos type values

data Either a b = Left a | Right b

Como vemos un Either es un tipo de dato que puede presentar valores de dos tipos, y uno y sólo por vez. Se comporta como una caja con dos compartimentos, pero usar uno inhabilita el otro y viceversa.
Entonces, como contenedor es bastante obvio: es aquel permite almacenar uno de dos valores. Y como computación, una función que devuelva Either representa a una computación que puede ser exitosa y entregar un resultado, o bien puede ocurrir un error y arrojar una excepción.
Por convención, el Left es el de error. Regla mnemotécnica: Right también significa correcto en inglés.

Para uso práctico podemos derivar de otros typeclases

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

para volver a nuestro ejemplo de la inversa es simple

```
inverse 0 = Left "Arithmetic Error: Division by zero"
inverse x = Right (1/x)
```

esto permite que podamos definir sitacuines de error y de que funciono todo, si bien no es un caso ideal, estamos cayendo de nuevo en algo similar a la idea de las continuations, cuando definimos una continuación en la que estaba todo bien y la que fallaba algo, y nos hace acordar un poco el Either a usar excepciones chequeadas en Java, pero veremos en la próxima clase de como podemos recuperar eso que teniamos en las excepciones de propagación de errores, ya que a este punto debemos incluso saber de que tipo es el error que se esta tirando si existe.

### Tipos de datos recursivos

Cuando vimos la lista no exploramos un poco más la idea de que son tipo de datos recursivos además 

volviendo a nuestro primer ejemplo

```haskell
data List a = Nil | Cons a (List a)
```

también podemos definirla en terminos de cabeza y cola, con derivaciones y record syntax

```haskell
data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
```

el Cons de nuevo es lo mismo que :, y en listas significa que es un constructor que toma un valor y una lista y devuelve otra lista. Entonces podemos ir concatenando valores a una lista de manera recursiva, la declaracion nos da ya el comportamiento para hacer un append, gratis!

Ahora veamos un ejemplo un poco más complejo pero con árboles, en estructura de datos vimos lo que era un arbol binario, es un arbol que por cada nodo que lo compone puede derivar en otros dos nodos a lo sumo, y cuando se inserta un valor al árbol, se compara el nodo raiz o superior, y si este valor es menor iremos por el nodo izquierdo y si es mayor al valor del nodo raiz por el derecho y así hasta llegar a un árbol vacío, y de ahi agregaremos un nodo en vez de un nodo vacío. Entonces esta es una estrutcura recursiva, por lo que si en un momento queremos una porción de un árbol binario, el que devolveremos es otro con dichos valores que queremos obetener, así para cuando agregamos un nodo al árbol no estaremos agregando un elemento al árbol existente sino que estaremos creando un nuevo árbol con un nodo más, esto es por lo que contamos al comienzo de la clase sobre inmutabilidad y transparencia referencial, no hay efecto de lado en

```
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
```

Entonces de nuevo cuando querramos agregar un valor a un arbol, estaremos generando uno nuevo, entonces tiene sentido que el contrato de un insertarNodo para un arbol binario, sea algo como lo siguiente:

a -> Tree a -> Tree a

```
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a left right)
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)
```

singletonTree  es algo que nos permite generar a partir de un valor del tipo a, un árbol con un nodo raiz y dos nodos izquierdo y derecho vacíos, esto es solo para el nuevo nodo. veamos que la función como otras ejecuta solamente por medio del valor de árbol que estamos evaluando, si es vació, es decir que llegamos al final del árbol y no hay más nodods y solo hay que generar el nuevo árbol, y sino hacer la comprobación de si estamos en un valor máyo o menor al valor que estamos insertando, si el valor es el mismo, solo "reemplazamos" el valor y generamos el nuevo árbol final. Tenemos otra función que es treeElem, que es para saber si existe un valor de tipo a, con un constraint que su tipo derive del typeclass Ord, en el árbol que también pasamos por parámetro, solo es para tener una función más, por ahora no es importante, solo veamos que la codificación es muy similar.

## Definiendo nuestras typeclases

Vimos que podíamos derivar nuestros tipos con typeclases, estos dijimos que son interfaces que nos permiten definir un comportamiento en particular, vimos ordenamiento, comparación, operaciones numéricas, y cuando derivamos en un tipo, este tipo puede comportarse como si fuera una instancia de estas typeclases. Esto tiene que ver a algo similar a las clases? La respuesta es no, esta noción no tiene nada que ver con las clases que conocemos del paradigma de objetos


https://en.wikibooks.org/wiki/Haskell/Classes_and_types

```
class  Eq a  where
   (==), (/=) :: a -> a -> Bool

       -- Minimal complete definition:
       --      (==) or (/=)
   x /= y     =  not (x == y)
   x == y     =  not (x /= y)
```

Esto es una implementación básica de Eq, class Eq a, es la definición de un typeclass llamado Eq, para el tipo a, esto significa que como las funciones a puede ser distintos tipos, no esta acotado a uno en particular, pero cuando se lo derive a se transformará en una instancia de Eq, para esa clase en particular.

veamos de crear una tipo de dato ahora:

```
data Semaforo = Verde | Amarillo | Rojo
```

Simple, ahora no derivamos, esto es porque vamos a crear una derivación a mano para la comparación, Eq, en vez de armar una derivación y que se use el caso base o definición mínima completa.

```
instance Eq Semaforo where
    Rojo == Rojo = True  
    Amarillo == Amarillo = True  
    Verde == Verde = True  
    _ == _ = False  
```

porque usamos instance y no class?, porque en este caso estamos definiendo Eq pero no para cualquier clase sino para una instancia de una clase del tipo Semáforo.


#### Al margen:

Para ver otras definciones de Eq, ver:

https://hackage.haskell.org/package/base-4.8.2.0/docs/Data-Eq.html

### Creando una typeclass

Finalmente armaremos una typeclass, hagamos una typeclass bien tonta, solo para entender como armar una, ya viendo a Eq, no debería ser dificil, ya que conocemos sintácticamente como hacerlo

En Haskell tenemos los booleanos, y en otros lenguajes vemos que en un if podemos preguntar algo como if (0) o if ("") o if (false), significan un mismo curso, por ejemplo en JS, podemos definir distintos resltados de un if dependiendo de lo que nos devuelva una función, por ej:

if (0) alert("YEAH!") else alert("NO!");
if ("") alert ("YEAH!") else alert("NO!"); 
if (false) alert("YEAH") else alert("NO!)

Y aunque en Haskell, un booleano es mucho mejor que esto que esta aca, ya que no necesitamos más que nuestras funciones sean consistentes y devuelvan un Boolean nada mas, armemos una basura al estilo JS

```haskell
class YeahNo a where
    yeahno :: a -> Bool

instance YeahNo Int where
    yeahno 0 = False  
    yeahno _ = True  
    
instance YeahNo [a] where
    yeahno [] = False  
    yeahno _ = True  
    
instance YeahNo Bool where
    yeahno = id   
    
instance YeahNo (Maybe a) where
    yeahno (Just _) = True  
    yeahno Nothing = False  
    
```

probemos esto y despues definamos una función YeahNoif

```
yeahnoIf :: (YesNo y) => y -> a -> a -> a  
yeahnoIf yeahnoVal todoBien fallo = if yesno yeahnoVal then todoBien else fallo
```

## Functores


### Categorias

La teoria de la categoria, dice que una ctaegoría debería tener un operador composición, que llamaremos (.)

La primera regla de la composición es la asociatividad:

(f . g) . h = f . (g . h) -- Associativity law

Esto es util ya que ignora completamiente el orden del agrupamiento, lease los parentesis que dan el orden, entonces podemos escribirlos sin los parántesis:

f . g . h

La teoría de composición también nos dice que debería tener una identidad a izquierda y derecha, que significa 

id . f = f  -- Left  identity law

f . id = f  -- Right identity law

La ley de aosiciatividad y las dos leyes de identidad son conocidas como las leyes de la categoria.

Pero no define que es (.), id o f, g y h. Sino que la teoría de la categoria nos permite descubrir que son. 

Veamos un poco más en detalle la categoría más simple que es la de las funciones.

```
id :: (a -> a)
id x = x
```

Esto es muy simple, es solo la funcion identidad, veamos de definir 

(.) y la composición en si:

```
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
```

si tenemos tiempo comprobemos que 

```
-- Identidad or izquierda
id . f = f 

= \x -> id (f x)
= \x -> f x
= f

-- Identidad por derecha

f . id
= \x -> f (id x)
= \x -> f x
= f

-- Asociatividad

(f . g) . h
= \x -> (f . g) (h x)
= \x -> f (g (h x))
= \x -> f ((g . h) x)
= \x -> (f . (g . h)) x
= f . (g . h)
```

### Functores desde el lado matematico...


Del lado más teórico, un functor transforma una categoría en otra, en la clase siguiente veremos que hay otras categoria que son las categorias monádicas, pero no nos desviemos.

la identidad de la categoria fuente se llamará idA, y su composición (.A)

asi la identidad de la categoria destino se llamará idB y su comoposicion (.B)

entonces las propiedades que tendran son 

```
idA .A f = f

f .A idA = f

(f .A g) .A h = f .A (f .A h) --Asociatividad
```

y para la categoria B

```
idB .B f = f

f .B idB = f

(f .B g) .B h = f .B (f .B h) --Asociatividad
```

luego un functor usas la funcion que llama a fmap para convertir cada componente de la categoría fuente en destino.

entonces fmap debe respetar dos reglas

- Debe transformar el operador de composición

```
map (f .A g) = map f .B map g
```

- fmap debe transformar la identidad de la categoria fuente en categoria destino

map idA = idB

si la categoria es la misma solo operará la función sobre los elementos que la componente, pero podemos incluso definir dentro de fmap la transformación a otra categoria, como por ej. una categoría de funcion a una monádica. Pero si la transformación es de una categoria, o sea de categoria funcion a otra categoria función, esto es lo que conocemos en Haskell como Functor class, que es un typeclass ya creada.

 
### Definicion práctica de Functor class

https://en.wikibooks.org/wiki/Haskell/The_Functor_class

veamos un poco lo que es un Functor, es básicamente una typeclass al que le podemos aplicar la operación map sobre tipos. Uno pensaría que puede hacerse en principio sobre listas, y esto es lo que generalmente se ve en la cursada de pdep, pero aca veremos que podemos también puede usarse para otros tipos, veamos lo que nos dice el wiki de haskell:

https://wiki.haskell.org/Functor

```
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```

cuando vamos a la parte de relacionarlo con la teoría de categorias, vemos que aplican las leyes para esta clase

```
fmap (f . g) = fmap f . fmap g  -- Composition law

fmap id = id                    -- Identity law
```

Veremos sobre esto un poco más adelante, solo quedemosnos con estas leyes en mente

Nos dice que el typeclass functor define fmap, que es interesante, pero lo más interesante es que el type argument f, es una función que toma un parámetro.

Entonces fmap toma una función que va de un a -> b, y un functor aplicado con un tipo, y devuelve un functor aplicado con otro tipo.

veamos un caso como map

```
Prelude> :t map
map :: (a -> b) -> [a] -> [b]
```

interesante, vean como toma tambien una función de a -> b, y una lista y devuelve otra lista, en realidad map es un caso particular de fmap, por lo que map es también un functor y map es un fmap definido para listas. 

```
instance Functor [] where
    fmap = map  
```

entonces si hacemos fmap sobre listas podremos usarlas? Claro que si

```
Prelude> fmap (*2) [1, 3 ]
[2,6]
Prelude> fmap (*2) [1]
[2]
Prelude> fmap (*2) []
[]
```

que pasa cuando hacemos un fmap sobre una lista, vacía? devuelve una lista vacía, esto es algo lógico, aunque convierte una lista de [a] en [b], siendo a y b del mismo tipo.

Aquellos tipos que puedan comportarse como una caja, ejemplo Maybe/Either, pueden ser functores. Entonces una lista la podemos ver como una caja con mucho compartimentos que pueden ser vacios, o con un valor. Que propiedades tienen estas cajas entonces? Por ejemplo, Maybe a, es una caja que vimos que puede tener algo adentro de ella, nada representado con Nothing, o un valor que podría ser 'Bleh', entonces el contenido de la caja sería Just 'Bleh', así es como Maybe es un functor

```
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
```

entonces veamos que lo que hicimos, es que cuando abrimos la caja, si es un Just x, tomamos el valor x, aplicamos el functor f sobre este valor y lo volvemos a meter en la caja dentro de un Just. Si en la caja no habia nada, tan solo la cerramos y seguimos que asi este. 

con el árbol sucede lo mismo

```
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
```


Volvamos un poco a lo que hablabamos de la teoría de categorías, los functores son un poco más que una funcion que puede aplicarse a un tipo que pueda comportarse como una caja, como es esto?
Veamos un ejemplo simple teórico, tenemos un par de funciones f y g:

```
f :: a -> b
g :: b -> c
```

pero tenemos que manipular listas del tipo 

```
h :: [a] -> [c]
```


en vez de reescribir una transformación de a -> c mediante un functor, podemos promover estas a listas y manejarla mediante el map que es el functor fmap definido ya:

si map es 


```
map :: (a -> b) -> ([a] -> [b])

map f :: [a] -> [b]
map g :: [b] -> [c]

h = map f . map g :: [a] -> [c]
```

esto se puede reducir a 


```
h = map (f . g) :: [a] -> [c]
```

y la lista vacia es ella misma que es similar a 

```
map id = id
```

de nuevo esto es igual que las leyes de categoria sobre Functor que vimos a comienzo de esta parte, entonces los functores a nivel general nos permite liberarnos de escribir código para una jerarquía monolítica de una categoria, en realidad, usamos el codigo de cualquier categoria que nos guste más y creamos apropiada y la promovemos para cambiarlo a otra categoría que necesitamos para escribir el código. Esto nos permite que trabajemos en categorías especializadas

Lo otro interesante es que nos permite extender estas para que si en el futuro, si hay una nueva categoría solo se tenga que definir nuevos functores para promover nuestro código al de la nueva categoria.

## Kind

Vimos a conmienzo de la clase que los valores tienen un tipo asociado, que puede verse como la etiqueta a lo que son, por ej "Hola MUndo" es un [Str], 2 es un Int, etc. Pero ahora veremos que los tipos tienen una etiqueta también, a las que llamaremos kind, por ej:

```
Prelude> :k Int
Int :: *
Prelude> :k [Char]
[Char] :: *
```

La * significa que Int se refiere a un tipo concreto, y esto es lo que representa Int y [Char], no hay transformaciones, sin embrargo:

```
Prelude> :k Maybe
Maybe :: * -> *
Prelude> :k Either
Either :: * -> * -> *
```

Son construcciones que nos permiten a partir de uno y dos tipos concretos generar un tipo concreto como resultado.

Veamos un ejemplo práctico,

armemos un typeclass gil:

```
class GilTypeC t where gil :: j x -> t x j
data Cajita a b = Cajita {estadoCajita :: b a} deriving (Show)
instance GilTypeC Cajita where gil x = Cajita x
```

no tiene mucho sentido derivar un tipo de dato Cajita de GilTypeC, y vemos que toma dos parámetros, la idea un poco es que podamos ver en que caso hay que tener en cuenta el kind de un typeclass y tipos cuando armamos typeclasses, si tenemos que j y x son de un tipo concreto, entonces x es * y j sería algo como * -> *, vemos tambien que t debería generar un tipo en concreto, entonces podemos inferir que t es del tipo * -> (* -> *) -> *, entonces solo necesitamos un x de tipo concreto y un j que vaya de un tipo concreto a otro, para que t genere un tipo concreto.

entonces con 

```
data Cajita a b = Cajita {estadoCajita :: b a} deriving (Show)
```

como sabemos que cajita es tiene un kind * -> (* -> *) -> * ?, Viendo la Definicion de tipo de datos, vemos que mantiene estado entonces guarda tipos concretos, de eso estamos seguros, aunque b puede ser una función de * -> * ya que estamos aplicando este con el valor a que sería del kind *.

veamos 

```
Prelude> :t Cajita { estadoCajita = Just 1 }
Cajita { estadoCajita = Just 1 } :: Num a => Cajita a Maybe
Prelude> :k Cajita
Cajita :: * -> (* -> *) -> * 
```

Siempre teniendo en cuenta que a es de un type constraint Num, tenemos que Cajita toma un a que es un kind concreto *, y Maybe es del kind (* -> *), y cajita es de un kind concreto *. Perfecto, entonces viendo :k tenemos algo muy similar, entonces hacer que Cajita use la typeclass GilTypeC es facil

```
instance GilTypeC Cajita where gil x = Cajita x
Prelude> gil (Just 1) :: Cajita Int Maybe
Cajita {estadoCajita = Just 1} 
```

Entonces cada vez que aplicamos un typeclass para un tipo de dato hay que tener en cuenta que e kind que exponga el typeclass pueda aplicarse al kind de dato

veamos por ej ahora con un tipo de dato cajita v2:

```
data CajitaComeback f a b = CajitaComeback { estado1 :: a, estado2 :: f b }
```

vemos que no deberían tener el mismo kind, o sea f debería tener un kind * -> *, mientras que a y b *, entonces viendo el kind

```
Prelude> :k CajitaComeback
CajitaComeback :: (* -> *) -> * -> * -> *
```

y si queremos que sea parte de Functor, no funcionaría ya que Functor acepta cosas del kind * -> *

```
Prelude> :k Functor 
Functor :: (* -> *) -> Constraint  
```

entonces para definir CajitaComeback, deberiamos hacer algo como

```
instance Functor CajitaComeback(a,b) where fmap f (CajitaComeback {estado1 = a, estado2 = b}) = CajitaComeback {estado1 = f a, estado2= b} 




