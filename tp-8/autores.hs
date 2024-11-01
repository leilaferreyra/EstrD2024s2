-- a) Implementar las siguientes funciones como usuario del TAD Organizador, establecer su eficiencia y justificarla:
-- a) 
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
-- programaron juntas.
programasEnComun p1 p2 org = intersection (programasDe p1 org) (programasDe p2 org)
-- b) 
esUnGranHacker :: Organizador -> Persona -> Bool
-- Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
esUnGranHacker org p = (nroProgramasDePersona org p) == length (todosLosProgramas org)

-- b) Implementar el TAD Organizador suponiendo el siguiente tipo de representación:
-- 
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
                       --map checksum                --map persona
{-Inv rep.: 
-Todas las personas de cada set asociado a un checksum en map checksum deben estar en el map de persona y viceversa.
-Cada checksum en el set de cheksum asociado a cada persona en map persona, debe existir en el map de cheksum y viceversa.
- Cada persona del set persona en map checksum, debe tener el mismo checksum asociado en map persona en su set checksum y viceversa.  
- 
 -}
-- a) Escribir los invariantes de representación para poder crear elementos válidos del TAD.
-- b) Implementar las funciones de la interfaz, respetando las restricciones de eficiencia pedidas. Justifique en cada caso por
-- qué se obtiene la eficiencia buscada.
-- c) Implementar una variante del TAD Organizador suponiendo que en la interfaz del TAD Organizador se agrega una nueva
-- operación:
-- 
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
agregarPrograma (MkO mc mp) c sp = MkO (assocM c sp mc) ()

asociarPersonasAChecksum 
-- Eficiencia: no hay ninguna garantía de eficiencia.
-- todosLosProgramas :: Organizador -> [Checksum]
-- Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
-- autoresDe :: Organizador -> Checksum -> Set Persona
-- Propósito: denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
-- programasDe :: Organizador -> Persona -> Set Checksum
-- Propósito: denota el conjunto de programas en los que participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
-- programaronJuntas :: Organizador -> Persona -> Persona -> Bool
-- Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
-- nroProgramasDePersona :: Organizador -> Persona -> Int
-- Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
