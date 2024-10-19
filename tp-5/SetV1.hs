module SetV1
      (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
     where
data Set a = S [a] Int 
                   --Cantidad de elementos de la estructura 
         
 {- INV. REP. : En S xs n 
                * xs no tiene repetidos                 
                * n es la longitud de la lista 
    -}

emptyS :: Set a -- Es 0(1)
emptyS = S [] 0 
--Crea un conjunto vacío.

addS :: Eq a => a -> Set a -> Set a --Es O(n) siendo n la cantidad de elementos de es. 
addS  e  (S es n) = if (elem e es)
                       then S es n
                       else S (e:es) (n+1)
--Dados un elemento y un conjunto, agrega el elemento al conjunto.


belongs :: Eq a => a -> Set a -> Bool -- Es O (n) siendo n la cantidad de elementos en es. 
belongs   e (S es n) =  e `elem` es 
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int -- Es O(1)
sizeS    (S _ n) = n 
--Devuelve la cantidad de elementos distintos de un conjunto.


removeS :: Eq a => a -> Set a -> Set a -- Es O(n) siendo n la cantidad de elementos en es
removeS    e (S es n) = if (elem e es )
                             then S (removeElement e es) (n-1)
                             else S es n

removeElement :: Eq a=> a -> [a] -> [a] ---- Es O(n) siendo n la cantidad de elementos en es
removeElement    _    [ ]      = []
removeElement    e1    (e2:es) = if (e1 == e2)
                                  then removeElement e1 es 
                                  else e2:es 
--Borra un elemento del conjunto.

unionS :: Eq a => Set a -> Set a -> Set a --Es O(n) siendo 
unionS    (S xs n1) (S ys n2 ) = let list = unirSinRepetidos xs ys 
                                      in (S list (length list)) 
                                        
unirSinRepetidos :: Eq a => [a] -> [a] -> [a]  --O(n2) siendo 
unirSinRepetidos [] ys = ys
unirSinRepetidos (x:xs) ys = if elem x ys  
                              then unirSinRepetidos xs ys 
                              else x : unirSinRepetidos xs ys 
 
-- O(n)
agregar x []     = [x]
agregar x (y:ys) = if (x==y) then y:ys else y:agregar x ys

-- O(n^2)
juntar []     ys = ys
juntar (x:xs) ys = agregar x (juntar xs ys) 
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a]
setToList   (S xs n) = xs 
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.

--2
--1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
--la cantidad de elementos en la estructura.
--Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
--de esta implementación, pero para mantener una interfaz común entre distintas posibles
--implementaciones estamos obligados a escribir así los tipos.
--Set (conjunto)