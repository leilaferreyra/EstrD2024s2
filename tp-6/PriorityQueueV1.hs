module PriorityQueueV1
       (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
    where 
data PriorityQueue a = PQ [a]

-- Ejercicio 1
-- La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
-- posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
-- Implementarla usando listas, e indicando el costo de cada operación.
emptyPQ :: PriorityQueue a --O(1)
-- Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool --O(1)
-- Propósito: indica si la priority queue está vacía.
isEmptyPQ  (PQ es) = null es

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a --O(1)
-- Propósito: inserta un elemento en la priority queue.
insertPQ e (PQ es) = PQ (e:es)


findMinPQ :: Ord a => PriorityQueue a -> a --O(n) siendo n la cantidad de elementos de es 
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
findMinPQ  (PQ es) = minimum es 
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a --O(n)
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
deleteMinPQ   (PQ es) = PQ (deleteElement (minimum es) es)


deleteElement :: Eq a => a -> [a] -> [a] --O(n)
deleteElement    _ [] = []
deleteElement   e (e1:es) = if e==e1
                             then es
                             else e1: deleteElement e es 
            
