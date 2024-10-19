-- 2. Map (diccionario)
module MapV1
     (Map, emptyM, assocM, lookupM,deleteM,keys)
     where

data Map k v = M [k] [v]
   deriving Show

{- INV.REP.: en M kvs, no hay claves repetidas. -}

-- Ejercicio 3
-- La interfaz del tipo abstracto Map es la siguiente:

emptyM :: Map k v --O(1)
-- Propósito: devuelve un map vacío
emptyM = M [] []

assocM :: Eq k => k -> v -> Map k v -> Map k v --O(n)
-- Propósito: agrega una asociación clave-valor al map.
assocM            x    y   (M xs ys) = let (ks, vs) = devolverTupla x y xs ys 
                                        in M ks vs 

devolverTupla :: Eq k => k -> v -> [k] -> [v] -> ([k], [v]) --O(n)
devolverTupla            x    y    [ ]    _    = ([x], [y])
devolverTupla            x' y' (x:xs) (y:ys)   = if x'== x 
                                                   then (x:xs, y':ys)
                                                   else agregarATupla x y (devolverTupla x' y' xs ys)

agregarATupla :: k -> v -> ([k], [v]) -> ([k], [v]) --O(1)
agregarATupla    x    y    ( xs, ys )  = (x:xs, y:ys)

lookupM :: Eq k => k -> Map k v -> Maybe v --O(n)
-- Propósito: encuentra un valor dado una clave
lookupM            x   (M xs ys) = valorAsociado x xs ys 

valorAsociado :: Eq k => k -> [k] -> [v] -> Maybe v --O(n)
valorAsociado            _    [ ]    _    = Nothing
valorAsociado            x (x':xs) (y:ys) = if x == x'
                                             then Just y 
                                             else valorAsociado x xs ys

deleteM :: Eq k => k -> Map k v -> Map k v --O(n)
-- Propósito: borra una asociación dada una clave.
deleteM            x  (M xs ys) = let (kv, vs) = deleteMTupla x xs ys 
                                    in M kv vs 

deleteMTupla :: Eq k=> k -> [k] -> [v] -> ([k], [v]) --O(n)
deleteMTupla           _    [ ]    _    = ([], [])
deleteMTupla           x (x':xs) (y:ys) = if (x == x')
                                           then deleteMTupla x xs ys 
                                           else agregarATupla x' y (deleteMTupla x xs ys)

keys :: Map k v -> [k]
-- Propósito: devuelve las claves del map.
keys (M kv vs) = kv 
mp :: Map Int String
mp = assocM 1 "lei" (assocM 2 "nati" (assocM 3 "effy" emptyM))

