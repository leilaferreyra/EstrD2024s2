import MapV1

data MultiSet a = MS (Map a Int)
    deriving Show

--    Un MultiSet (multiconjunto) es un tipo abstracto de datos similar a un Set (conjunto). A diferencia
-- del último, cada elemento posee una cantidad de apariciones, que llamaremos ocurrencias del
-- elemento en el multiset. Su interfaz es la siguiente:
emptyMS :: MultiSet a
-- Propósito: denota un multiconjunto vacío.
emptyMS = MS (emptyM)
addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS x (MS mp) = case lookupM x mp of
                    Nothing -> MS (assocM x 1 mp)
                    Just v ->  MS (assocM x (v+1) mp)

fromJust :: Maybe v -> v
fromJust (Just v) = v
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
ocurrencesMS x (MS mp)= case of lookupM x mp 
                         Nothing -> 0
                         Just v -> v 

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a (opcional)
-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
unionMS (MS mp1) (MS mp2) = Ms (unionMaps mp1 mp2) 

unionMaps :: Ord a => Map a Int -> Map a Int -> Map a Int
unionMaps    mp1 mp2 =  unirTupla (mapToList mp1) mp2 

unirTupla :: Ord a => [(k,v)] -> Map a Int -> Map a Int
unirTupla     [] mp = mp
unirTupla    ((k,v):kvs) mp = unirTupla kvs (assocM k (fromMaybe 0 (lookupM k mp)) mp)

mapToList :: Eq a => Map a Int -> [(a, Int)]
-- Propósito: convierte un map en una lista de pares clave valor.
mapToList  mp = mapToTuplas (keys mp) mp 

mapToTuplas :: Eq a => [a] -> Map a Int-> [(a,Int)]
mapToTuplas    [] _ = []
mapToTuplas    (x:xs) mp = (x, fromJust(lookupM x mp)) : mapToTuplas xs mp

fromMaybe :: a -> Maybe a -> a
fromMaybe defaultVal maybeVal =
  case maybeVal of
    Nothing -> defaultVal
    Just val -> val


--intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.



multiSetToList :: MultiSet a -> [(a, Int)]
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList (MS mp) = mapToList mp 

-- mapToList :: Eq k => Map k v -> [(k, v)]
-- -- Propósito: convierte un map en una lista de pares clave valor.
-- mapToList  mp = mapToTuplas (keys mp) mp 

-- mapToTuplas :: Eq k => [k] -> Map k v -> [(k,v)]
-- mapToTuplas    [] _ = []
-- mapToTuplas    (x:xs) mp = (x, fromJust(lookupM x mp)) : mapToTuplas xs mp