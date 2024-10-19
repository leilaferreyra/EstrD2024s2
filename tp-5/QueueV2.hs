module QueueV2
       (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
       where 
data Queue a = Q [a]

--Implemente ahora la versión que agrega por delante y quita por el nal de la lista. Compare
--la eciencia entre ambas implementaciones.

emptyQ :: Queue a --O(1)
emptyQ = Q [] Int 
--Crea una cola vacía.
isEmptyQ :: Queue a -> Bool --O(1)
isEmptyQ  (Q es n) =  n == 0 
--Dada una cola indica si la cola está vacía.
enqueue :: a -> Queue a -> Queue a --O(1)
enqueue    x    (Q xs n) = Q (x:xs) (n+1)
--Dados un elemento y una cola, agrega ese elemento a la cola.
--PRECONDICION: La cola no puede estar vacia.
firstQ :: Queue a -> a --  --O(1)
firstQ   (Q xs) = last  xs
--Dada una cola devuelve el primer elemento de la cola.
--PRECONDICION: La cola no puede estar vacia. 
dequeue :: Queue a -> Queue a -- O(n) siendo n la cantidad de elementos de xs 
dequeue   (Q xs) = Q (init xs)

