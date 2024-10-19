module StackV1
       (Stack, emptySt, isEmptyS, push, top, pop, lenS)
       where 
data Stack a = S [a] Int 

--4. Stack (pila)
--Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, rst out). Esto signica
--que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
--platos). Su interfaz es la siguiente:
emptySt :: Stack a
emptySt = S [] 0 
--Crea una pila vacía.
isEmptyS :: Stack a -> Bool
isEmptyS    (S xs _) = null xs 
--Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a
push   x (S xs n) = S (x:xs) (n+1)

--Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a
top   (S xs _) = head xs 

--Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a
pop    (S xs n) = S (init xs) n
--Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int
lenS    (S xs n ) = n
--Dada la cantidad de elementos en la pila.
--Costo: constante