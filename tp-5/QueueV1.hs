module QueueV1
       (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
       where 
data Queue a = Q [a]

--3. Queue (cola)
--Una Queue es un tipo abstracto de datos de naturaleza FIFO (rst in, rst out). Esto signica
--que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
--primero en salir (como la cola de un banco). Su interfaz es la siguiente:

--1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
--nal de la lista y desencolarse por delante.

emptyQ :: Queue a -- O(1)
emptyQ = Q [] 
--Crea una cola vacía.

isEmptyQ :: Queue a -> Bool --O(1)
isEmptyQ  (Q xs) =  null xs
--Dada una cola indica si la cola está vacía.

enqueue :: a -> Queue a -> Queue a --O(n) siendo n la cantidad de elementos de xs 
enqueue    x    (Q xs) = Q (xs ++ [x])  
--Dados un elemento y una cola, agrega ese elemento a la cola.

--PRECONDICION: La cola no puede estar vacia.
firstQ :: Queue a -> a -- O(1)
firstQ   (Q xs) = head xs
--Dada una cola devuelve el primer elemento de la cola.

--PRECONDICION: La cola no puede estar vacia. 
dequeue :: Queue a -> Queue a --O(1)
dequeue   (Q xs) = Q (tail xs)
--Dada una cola la devuelve sin su primer elemento.




