data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
-- INV. REP. : - Un tripulante coN valor en el segundo map tiene que estar en la hip ,
-- si hay un tripulante en la hip tiene que estar como valor en els egundo map
-- el nombre como clave en el segundo tiene que se rigual al nombre del valor ??
-- la clave del primer map esta asociada a  un sector, esa clave pertenece a dicho sector id 
-- .... 

construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S)
construir xs = N (construirM xs) emptyM emptyH

construirM :: [SectorId] -> Map SectorId Sector
construirM [] = emptyM
construirM   (x:xs) = assocM x (crearS x) (construirM xs)

ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Eficiencia: O(log T)
ingresarT n r (N sm tm th) = case lookupM n tm of 
                               Nothing -> let newT = crearT n r 
                                            in N (sm (ingresarTM newT tm) (insertH newT)
                                Just v -> (N sm tm )

ingresarTM :: Tripulante -> Map Nombre Tripulante -> Map Nombre Tripulante
ingresarTM    t mp = assocM (nombre t) t mp 
-- Sector, siendo C la cantidad de contenedres y T la cantidad de tripulantes:
-- crearS :: SectorId -> Sector O(1)
-- sectorId :: Sector -> SectorId O(1)
-- componentesS :: Sector -> [Componente] O(1)
-- tripulantesS :: Sector -> Set Nombre O(1)
-- agregarC :: Componente -> Sector -> Sector O(1)
-- agregarT :: Nombre -> Sector -> Sector O(log T)
-- Tripulante, siendo S la cantidad de sectores:
-- crearT :: Nombre -> Rango -> Tripulante O(1)
-- asignarS :: SectorId -> Tripulante -> Tripulante
-- O(log S)
-- sectoresT :: Tripulante -> Set SectorId O(1)
-- nombre :: Tripulante -> String O(1)
-- rango :: Tripulante -> Rango O(1)
-- Set, siendo N la cantidad de elementos del conjunto:
-- emptyS :: Set a O(1)
-- addS :: a -> Set a -> Set a O(log N)
-- belongsS :: a -> Set a -> Bool O(log N)
-- unionS :: Set a -> Set a -> Set a O(N log N)
-- setToList :: Set a -> [a] O(N)
-- sizeS :: Set a -> Int O(1)
-- MaxHeap, siendo M la cantidad de elementos en la heap:
-- emptyH :: MaxHeap a O(1)
-- isEmptyH :: MaxHeap a -> Bool O(1)
-- insertH :: a -> MaxHeap a -> MaxHeap a O(log M)
-- maxH :: MaxHeap a -> a O(1)
-- deleteMaxH :: MaxHeap a -> MaxHeap a O(log M)
-- Map, siendo K la cantidad de claves distintas en el map:
-- emptyM :: Map k v O(1)
-- assocM :: k -> v -> Map k v -> Map k v O(log K)
-- lookupM :: k -> Map k v -> Maybe v O(log K)
-- deleteM :: k -> Map k v -> Map k v O(log K)
-- domM :: Map k v -> [k] 