import navesUsuario 

-- Implementar las siguientes funciones como usuario del tipo Nave, indicando la eficiencia obtenida para cada operación:
-- i) 
sectores :: Nave -> Set SectorId
-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores n =  sectoresT (tripulantesN n) n 

sectoresT :: [Tripulante] -> Nave -> Set SectorId 
sectoresT    [] n = emptyS
sectoresT    (t:ts) n = unionS (sectoresAsignados (nombre t) n) sectoresT ts n 
-- j) 
sinSectoresAsignados :: Nave ->[Tripulante]
-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados n = sinSectoresAsignados' (tripulantesN n) n 

sinSectoresAsignados' :: [Tripulante] -> Nave -> [Tripulante]
sinSectoresAsignados' [] n = []
sinSectoresAsignados' (t:ts) n = if sizeS (sectoresAsignados (nombre t) n) == 0
                                then t : sinSectoresAsignados' ts n
                                else sinSectoresAsignados' ts n 

-- k) 
barriles :: Nave -> [Barril]
-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
-- Bonus
barriles n = let componentes = componentesDeN n (sectores n)
               in agregarBarriles componentes 

agregarBarriles :: [Componente] -> [Barril]
agregarBarriles  [] = []
agregarBarriles  (c:cs) = if esAlmacen c 
                            then barrilesDeM ++ agregarBarriles cs
                            else agregarBarriles cs 

componentesDeN :: Nave -> [SectorId] -> [Componente]
componentesDeN   n    [] = []
componentesDeN  n (s:ss) = scn (datosDeSector s n) ++ componentesDeN n ss 

barrilDelAlmacen :: Componente -> [Barril]
--Constante
barrilDelAlmacen (Almacen b) = b

-- l) Dar una posible representación para el tipo Sector, de manera de que se pueda cumplir con el orden dado para cada
-- operación de la interfaz, pero sin implementarlas.

data Sector = S SectorId (Set Tripulante) [Componente]

