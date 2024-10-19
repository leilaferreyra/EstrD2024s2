module Empresa 
import MapV1
import SetV1 
   (ConsE, constEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores,agregarSector,agregarEmpleado)
   where 
type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)

data Empleado = E Int (Set SectorId)
                 --CUIL --SECTORES A LOS QUE PERTENECE.
consEmpleado :: CUIL -> Empleado --O(1)
-- Propósito: construye un empleado con dicho CUIL.
-- Costo: O(1)
consEmpleado c = E c emptyS

-- 
cuil :: Empleado -> CUIL
-- Propósito: indica el CUIL de un empleado.
-- Costo: O(1)
cuil (E c _) = c 
-- 
incorporarSector :: SectorId -> Empleado -> Empleado
-- Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
-- Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector s (S c ss) = S c (addS s ss)
-- 
sectores :: Empleado -> [SectorId]
-- Propósito: indica los sectores en los que el empleado trabaja.
-- Costo: O(S)
sectores (E _ ss) = setToList ss 

{- INV. REP.:
   --Un empleado puede trabajar en mas de un sector id. 
   --El segundo map relacion el cuil de cada empleado con el empleado. 
   --El primer map relaciona id  de sectores con los empleados que trabajan en dicho sector. 
   --No hay sectores id repetidos. 
   --Cada empleado que trabaje en un sector id debera pertenecer al set de empleados de dicho sector id
   --Los sectores deben ser los mismos que los de las listas que tiene cada empleado.    -}
consEmpresa :: Empresa
-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa = ConsE emptyM emptyM
-- 
buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- Propósito: devuelve el empleado con dicho CUIL.
-- Precondición: el CUIL es de un empleado de la empresa.
-- Costo: O(log E)
buscarPorCUIL c (ConsE _ es) = lookupM c es 
-- 
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(log S + E)
empleadosDelSector sector (ConsE ss _) = case lookupM sector ss of 
                                       Just v -> setToList v 
                                       Nothing -> error "El sector no pertenece a la empresa dada"
-- 
todosLosCUIL :: Empresa -> [CUIL]
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosCUIL (ConsE ss es) = keys es 
-- 
todosLosSectores :: Empresa -> [SectorId]
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
todosLosSectores  (ConsE ss es) = keys ss 
-- 
agregarSector :: SectorId -> Empresa -> Empresa
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S)
agregarASector id (E ss es) = E (assocM id emptyS ss) es
-- 
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el
-- CUIL dado.
-- Costo: O (S log E) ?
agregarEmpleado is cuil (E ss es) = let emp = incorporarASectores (consEmpleado ss) in 
                            in (E  (agregarEmpleadoASectores emp is ss) (incorporarAEmpleados emp es))

incorporarAEmpleados :: Empleado -> Map CUIL Empleado -> Map CUIL Empleado 
incorporarAEmpleados emp mp = assocM (cuil emp) emp mp -- O(log E)?

agregarEmpleadoASectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoASectores    _ [] mp     = mp
agregarEmpleadoASectores    e (x:xs) mp = case lookupM x mp of 
                                           Just v -> (assocM x (addS e v)) agregarEmpleadoASectores e xs mp --O(log S) ?
                                           Nothing -> (assocM (addS emptyS)) agregarEmpleadoASectores e xs mp 

incorporarASectores :: Empleado -> [SectorId] -> Empleado
-- O (S log S) 
incorporarASectores   e [] = e 
incorporarASectores   e (i:is) =  incorporarSector i (incorporarASectores e is)

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: calcular
agregarASector    id cuil (ConsE ss es) = case lookupM cuil es of
                                           Just v -> let empleado = incorporarSector id v in 
                                                       in ConsE (agregarASectorM id empleado ss) (incorporarEmpleado empleado mp)
                                           Nothing -> ConsE ss es

incorporarEmpleado:: Empleado -> Map CUIL Empleado -> Map CUIL Empleado
incorporarEmpleado    emp     mp = assocM (cuil emp) emp mp --O(log E) ?

agregarASectorM :: SectorId -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarASectorM    id e mp =   assocM id (addS e fromJust((lookupM id mp))) mp --O(log S) o O(S) ??

borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: 
borrarEmpleado cuil (ConsE ss es) = case lookupM cuil es of 
                                      Just v -> ConsE (borrarEmpleadoDeSectores (keys ss) v ss) (borrarEmpleadoM cuil es)
                                      Nothing -> ConsE ss es 

borrarEmpleadoM:: CUIL -> Map CUIL Empleado --O log E 
borrarEmpleadoM cuil mp = deleteM cuil mp 

borrarEmpleadoDeSectores:: [SectorID] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (SetEmpleado)
borrarEmpleadoDeSectores [] _ mp =  mp
borrarEmpleadoDeSectores (x:xs) emp mp = case lookupM x mp of 
                                          Just v -> (assocM x (borrarEmpleadoS emp v) ) borrarEmpleadoDeSectores xs emp mp
                                          Nothing -> borrarEmpleadoDeSectores xs emp mp 

borrarEmpleadoS:: Empleado -> Set Empleado -> Set Empleado 
borrarEmpleadoS emp set = removeS emp set --O(E)

fromJust :: Maybe b -> a --O(1)
fromJust Just v = v 