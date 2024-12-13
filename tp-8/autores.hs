--USUARIO 
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
-- programaron juntas.
-- O (log P + P log P)
programasEnComun p1 p2 org =  intersection (programasDe p1 org) (programasDe p2 org)
-- b)
esUnGranHacker :: Organizador -> Persona -> Bool
-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
--Eficiencia : O(C + log P)
esUnGranHacker org p = (nroProgramasDePersona org p) == length (todosLosProgramas org)
--IMPLEMENTADOR 
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
                       --map checksum                --map persona
{-Inv rep.: 
-- Para todo Checksum clave del primer map, en el caso de no ser vacio, cada Persona de su set persona valor 
tiene que ser una Persona clave en el segundo map, con ese Checksum integrando su set de checksum valor. 
-- Para toda Persona clave del segundo map, cada Checksum de su Set Checksum valor tiene que ser una clave Checksum
en el primer map, con esa Persona integrando su Set Persona valor.  
 -}

elMayorPrograma :: Organizador -> Maybe Checksum
-- Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
-- Nothing si no puede devolver un programa.
-- Eficiencia: O(1) en peor caso.
elMayorPrograma (MkO mc mp) = 
-- Esto puede requerir modificar el tipo de representación, agregar invariantes, y modificar operaciones existentes. Reescribir
-- sólo las operaciones que tienen cambios sustanciales y no en las que, por ejemplo, sólo se modifica un pattern matching.
-- 
nuevo :: Organizador
-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo = MkO emptyM emptyM 
-- 
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
-- Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
agregarPrograma (MkO mc mp) c sp = let checksumActualizado = assocM c sp mc
                                       personasActualizado = asociarPersonasAChecksum setToList sp c mp
                                       in MkO checksumActualizado personasActualizado

asociarPersonasAChecksum :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
asociarPersonasAChecksum [] c mp = mp 
asociarPersonasAChecksum (p:ps) c mp = case lookM p mp of 
                                        Just st -> asociarPersonasAChecksum ps c (assocM p (addS c st) mp)
                                        Nothing -> asociarPersonasAChecksum ps c (assocM p (addS c emptyS) mp)

todosLosProgramas :: Organizador -> [Checksum]
-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (Mko mc mp) = keys mc

autoresDe :: Organizador -> Checksum -> Set Persona
-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (Mko mc mp) c = fromJust (lookM c mc)

programasDe :: Organizador -> Persona -> Set Checksum
-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (Mko mc mp) p = fromJust (lookM p mp)
-- 
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
programaronJuntas (MkO mc mp) p1 p2 = let programasdeP1 = case lookM p1 mp of 
                                                           Just st -> st 
                                                           Nothing -> emptyS
                                          programasdeP2 = case lookM p2 mp of
                                                            Just st -> st 
                                                            Nothing -> emptyS 
                                        in not (isEmpty (intersection programasdeP1 programasdeP2))
-- 
nroProgramasDePersona :: Organizador -> Persona -> Int
-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona (Mko mc mp) p = case lookupM p mp of
                                        Just sc -> sizeS sc 
                                        Nothing -> 0 
