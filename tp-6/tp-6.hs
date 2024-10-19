import PriorityQueueV1
import MapV1 
import Data.Maybe (fromJust)

-- 1. Priority Queue (cola de prioridad)
-- Ejercicio 2
-- Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
-- menor a mayor utilizando una Priority Queue como estructura auxiliar. ¾Cuál es su costo?
-- OBSERVACIÓN: el nombre heapSort se debe a una implementación particular de las Priority
-- Queues basada en una estructura concreta llamada Heap, que será trabajada en la siguiente
-- práctica.

heapSort :: Ord a => [a] -> [a]
heapSort   [] = []
heapSort   es =  let pq = listToPQ es 
                 in findMinPQ pq : heapSort (pqToList(deleteMinPQ pq))

pqToList :: Ord a => PriorityQueue a -> [a]
pqToList    pq = if isEmptyPQ pq 
                  then []
                  else findMinPQ pq : pqToList (deleteMinPQ pq)

listToPQ :: Ord a => [a] -> PriorityQueue a
listToPQ    [] = emptyPQ
listToPQ    (e:es) = insertPQ e (listToPQ es)

pq :: PriorityQueue Int
pq = (insertPQ 2 (insertPQ 5 (insertPQ 1 (emptyPQ))))
-- Implementar como usuario del tipo abstracto Map las siguientes funciones:
-- 1. 
valuesM :: Eq k => Map k v -> [Maybe v]
-- Propósito: obtiene los valores asociados a cada clave del map.
valuesM  mp = valoresM' (keys mp) mp

valoresM':: Eq k => [k] -> Map k v -> [Maybe v]
valoresM'    [] _      = []
valoresM'    (x:xs) mp = (lookupM x mp) : valoresM' xs mp

mp :: Map Int String
mp = assocM 1 "lei" (assocM 2 "nati" (assocM 3 "effy" emptyM))
-- 2. 
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
-- Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas xs mp = todasAsociadas' xs (keys mp)

todasAsociadas' :: Eq k => [k] -> [k] -> Bool
todasAsociadas'    [] _ = True 
todasAsociadas'   (x:xs) ys = (elem x ys) && todasAsociadas' xs ys 
-- 3. 
listToMap :: Eq k => [(k, v)] -> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.
listToMap    [ ]         = emptyM 
listToMap    ((x,y):xys) = (assocM x y) (listToMap xys) 
-- 4. 
mapToList :: Eq k => Map k v -> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.
mapToList  mp = mapToTuplas (keys mp) mp 

mapToTuplas :: Eq k => [k] -> Map k v -> [(k,v)]
mapToTuplas    [] _ = []
mapToTuplas    (x:xs) mp = (x, fromJust(lookupM x mp)) : mapToTuplas xs mp
-- 5. 
agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.
-- Costo cúbico?
agruparEq kvs  = asociarEq (agruparEq' kvs)
   
asociarEq :: Eq k => [(k, [v])] -> Map k [v]
-- Cuadrática
asociarEq []        = emptyM
asociarEq ((k,v):kvs)  = assocM k v (asociarEq kvs)

agruparEq' :: Eq k => [(k, v)] -> [(k, [v])]
-- Cuadrática
agruparEq' []        = []
agruparEq' (kv:kvs)  = juntarValores kv (agruparEq' kvs)
    
juntarValores :: Eq k => (k, v) -> [(k, [v])] -> [(k, [v])]
-- Lineal
juntarValores (k,v)  []      = [(k, [v])]
juntarValores (k,v) ((k',v'):kvs) = if k==k'
                                   then (k', v:v') : kvs
                                   else (k',v') : juntarValores (k,v) kvs

-- 6. 
incrementar ::Eq k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
-- cada número asociado con dichas claves.
incrementar [] mp = mp 
incrementar (x:xs) mp = incrementar xs (assocM x (fromJust(lookupM x mp) + 1) mp)

-- 7. 
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
-- Indicar los ordenes de complejidad en peor caso de cada función implementada, justicando
-- las respuestas.
mergeMaps mp1 mp2 = asociarListMaps (mapToList mp1) mp2 

asociarListMaps :: Eq k => [(k,v)] -> Map k v -> Map k v 
asociarListMaps  [] mp = mp
asociarListMaps  ((k,v):kvs) mp = asociarListMaps kvs (assocM k v mp)

indexar :: [a] -> Map Int a
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar xs = indexarAux xs 0 

indexarAux :: [a] -> Int -> Map Int a
indexarAux [] _ = emptyM
indexarAux (x:xs) n = assocM n x (indexarAux xs (n + 1))

ocurrencias :: String -> Map Char Int
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias [] = emptyM 
ocurrencias (x:xs) = assocM x ((apariciones x xs)+1)(ocurrencias xs)

apariciones :: Eq a => a -> [a]   -> Int
apariciones            _    [ ]    =  0
apariciones            e    (x:xs) = unoSi (e==x) + apariciones e xs

unoSi :: Bool -> Int
unoSi    True = 1 
unoSi    False = 0

