-- Nave Espacial
-- En este examen modelaremos una Nave como un tipo abstracto, el cual nos permite construir una nave espacial, dividida en
-- sectores, a los cuales podemos asignar tripulantes y componentes. Para esto, damos por hecho que:
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible
-- El tipo Sector es un tipo abstracto, y representa al sector de una nave, el cual contiene componentes y tripulantes asignados.
-- El tipo Tripulante es un tipo abstracto, y representa a un tripulante dentro de la nave, el cual tiene un nombre, un rango
-- y sectores asignados.
-- El tipo SectorId es sinónimo de String, e identifica al sector de forma unívoca.
-- Los tipos Nombre y Rango son sinónimos de String. Todos los nombres de tripulantes son únicos.
-- Un sector está vacío cuando no tiene tripulantes, y la nave está vacía si no tiene ningún tripulante.
-- Puede haber tripulantes sin sectores asignados.
-- Representación
-- Dicho esto, la representación será la siguiente (que no es posible modificar ):
data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
               --Sectores map            --Tripulantes map      --tripulantes heap 
{-INV. REP.:
-Todos los tripulantes de tripulantes map deben existir en tripulantes heap y viceversa. 
-Si un tripulante de tripulantes map y tripulantes heap tiene un sector o mas asignados, debe estar en el map de sectores asignado a todos
los sectores correspondientes.
-En tripulantes map, el Nombre debe coincidir con el nombre del tripulante.
-En tripulantes map y tripulantes heap el rango del tripulante debe coincidir. 
-En Sectores map el SectorId debe coincidir con el sectorId del Sector. 
-}
-- Esta representación utiliza:
-- Un Map que relaciona para cada SectorId su sector correspondiente.
-- Otro Map que relaciona para cada Nombre de tripulante el tripulante con dicho nombre.
-- Una MaxHeap que incluye a todos los tripulantes de la nave, cuyo criterio de ordenado es por rango de los tripulantes.
-- Ejercicios
-- Invariantes
-- a) Dar invariantes de representación válidos según la descripción de la estructura.
-- Implementación
-- Implementar la siguiente interfaz de Nave, utilizando la representación y los costos dados, calculando los costos de cada
-- subtarea, y siendo T la cantidad de tripulantes y S la cantidad de sectores:
-- b) 
construir :: [SectorId] -> Nave
-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S) 
construir sectores = N (crearSectores sectores) emptyM emptyH 

-- Lineal, por cada sector id de la lista hago asoccM (log S), crearS cuesta O(1)
crearSectores :: [SectorId] -> Map SectorId Sector 
crearSectores [] = emptyM 
crearSectores (s:ss) = assocM s (crearS s) crearSectores ss 

-- c) 
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT    nombre rango (N s tm th) = let newT = crearT nombre rango in 
                                             N s (assocM nombre newT tm) (insertH newT th)
                                         
-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T) asociar un tripulante al map de tripulante utilizando assocM vale log (T) y asociarlo al heap de tripulantes utilizando insertH log (T)
-- d) 
sectoresAsignados :: Nombre -> Nave -> Set SectorId
-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log M). Saber los sectores de un tripulante vale O(1) y buscar un tripulante en el map de tripulantes utilizando lookup vale log T
-- utilizar fromJust vale O(1)
sectoresAsignados n (N s tm  th) = let tripulante = fromJust (lookM n tm) 
                                     in sectoresT tripulante 

fromJust :: Maybe value -> value
fromJust   Just v = v 
-- e) 
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S) tripulantesS cuesta O(1), componentesS cuesta O(1) y buscar un sector en el map de sectores con lookupM cuesta (logS)
datosDeSector id (N s tm th) = let sector = fromJust (lookM id s)
                                in (tripulantesS sector, componentesS sector)
                        
-- f) 
tripulantesN :: Nave -> [Tripulante]
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T) buscar un tripulante en el heap de tripulantes utilizando maxH cuesta log T y
-- borrarlo cuesta log T (RARO) 
tripulantesN (N _ _ th) = tripulantesH th 

tripulantesH :: MaxHeap Tripulante -> [Tripulante]
tripulantesH th = if isEmptyH th
                   then []
                   else maxH th : tripulantesH (deleteMaxH th)
-- g) 
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados. agregarCASector cuesta C (lineal) y agregarComponentes cuesta log S
agregarASector cs id (N s tm th) = N (agregarComponentes cs id s) tm th

agregarComponentes :: [Componente] -> SectorId -> Map SectorId Sector  -> Map SectorId Sector
--O log S
agregarComponentes   [] id sm = sm 
agregarComponentes   cs id sm = case lookM id sm of 
                                    Just s -> assocM id (agregarCASector cs s) sm
                                    Nothing -> sm 
                                   
agregarCASector :: [Componente] -> Sector -> Sector 
-- O(C)
agregarCASector   []    sec = sec 
agregarCASector  (c:cs) sec = agregarC c (agregarCASector cs sec)
-- h) 
asignarASector :: Nombre -> SectorId -> Nave -> Nave
-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen.
-- Eficiencia: O(log S + log T + T log T)
asignarASector n id (N s tm th) = let tripulanteActualizado = asignarS id (fromJust (lookupM n tm))
                                      in N (actualizarSector n id s) (assocM n tripulanteActualizado tm) (insertH tripulanteActualizado th)
                                            --log S                   --log T                              --T log T 
actualizarSector :: Nombre -> SectorId -> Map SectorId Sector -> Map SectorId Sector
-- log S 
actualizarSector n id mp = let sectorActualizado = agregarT n (fromJust (lookM id mp))
                             in assocM id sectorActualizado mp 
-- Usuario
