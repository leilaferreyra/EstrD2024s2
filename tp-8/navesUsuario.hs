
-- Implementar las siguientes funciones como usuario del tipo Nave, indicando la eficiencia obtenida para cada operación:
-- i) 
sectores :: Nave -> Set SectorId
-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
--Eficiencia: O (T log T + T * (S log S + log T) ). 
-- O (T log T + #  Por tripulantesN. 
--    T *(S log S + logT)  # por sectoresT (Notar que T* es la cantidad de tripulantes de la nave.)
sectores n =  sectoresT (tripulantesN n) n 

sectoresT :: [Tripulante] -> Nave -> Set SectorId 
--Precondicion: Todos los tripulantes estan en la nave. 
--Eficiencia: O (T * (S log S + logT)). 
--Obtener el nombre de un tripulante cuesta O(1)
--Crear un set vacio cuesta O(1)
--unir los sectoresId utilizando unionS cuesta S log S.
--Obtener los sectoresAsignados de un tripulante en la nave cuesta log T. 
-- O (T * #Por recursion de la lista de Tripulante provista (Notar que T es arbitrario, podria ser otra variable)
--     (log T +  # Por sectores asignados. 
--      S log S )) # Por unionS en sets que tienen en el peor de los casos tamaño igual a la cantidad de sectores de la nave.
--   => Eficiencia: O (T * (S log S + logT)). 
sectoresT    [] n = emptyS
sectoresT    (t:ts) n = unionS (sectoresAsignados (nombre t) n) sectoresT ts n 
-- j) 
sinSectoresAsignados :: Nave ->[Tripulante]
-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
--Eficiencia O ( T log T )
sinSectoresAsignados n = sinSectoresAsignados' (tripulantesN n) n 

sinSectoresAsignados' :: [Tripulante] -> Nave -> [Tripulante]
sinSectoresAsignados' [] n = []
sinSectoresAsignados' (t:ts) n = if not tieneSectoresAsignados t n 
                                then t : sinSectoresAsignados' ts n
                                else sinSectoresAsignados' ts n 

tieneSectoresAsignados :: Tripulante -> Nave -> Bool
--Eficiencia : log T 
tieneSectoresAsignados t n = sizeS (sectoresAsignados (nombre t) n) > 0 
-- k) 
barriles :: Nave -> [Barril]
-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
-- Bonus
barriles n = let componentes = componentesDeN n (sectores n)
               in agregarBarriles componentes 

componentesDeN :: Nave -> [SectorId] -> [Componente]
componentesDeN   n    [] = []
componentesDeN  n (s:ss) = scn (datosDeSector s n) ++ componentesDeN n ss 

agregarBarriles :: [Componente] -> [Barril]
agregarBarriles  [] = []
agregarBarriles  (c:cs) = barrilesDelAlmacen c ++ agregarBarriles cs
    
barrilDelAlmacen :: Componente -> [Barril]
--Constante
barrilDelAlmacen (Almacen b) = b
barrrilDelAlmacen _          = []


