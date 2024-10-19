module SetV2
      (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
     where
data Set a = S [a] 


emptyS :: Set a --O(1)
emptyS = S [] 
--Crea un conjunto vacío.
addS :: Eq a => a -> Set a -> Set a --O(1)
addS  x  (S xs) = S (x:xs) 

--Dados un elemento y un conjunto, agrega el elemento al conjunto.
-- 
belongs :: Eq a => a -> Set a -> Bool -- O(n) siendo n la cantidad de elementos en es 
belongs   e (S es) = elem e es 
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.

sizeS :: Eq a => Set a -> Int --Preguntar si se precisa la n en la declaracion de set o no . 
sizeS    (S (xs)) = length (eliminarRepetidos xs)
--Devuelve la cantidad de elementos distintos de un conjunto.
removeS :: Eq a => a -> Set a -> Set a --O(n2) siendo n la cantidad de elementos de la lista es. ???
removeS    e (S es) =   S (removeElement e es)

removeElement :: Eq a=> a -> [a] -> [a] --O(n) siendo n la cantidad de elementos en es. 
removeElement    _    [ ]      = []
removeElement    e1    (e2:es) = if (e1 == e2)
                                  then removeElement e1 es 
                                  else e2 : (removeElement e1 es) 
--Borra un elemento del conjunto.
unionS :: Eq a => Set a -> Set a -> Set a --O(n2)
unionS    (S xs n1) (S ys n2 ) = let list = unirSinRepetidos xs ys 
                                      in (S list)
                                        
unirSinRepetidos :: Eq a => [a] -> [a] -> [a] --O(n)
unirSinRepetidos [] ys = ys
unirSinRepetidos (x:xs) ys = if elem x ys  
                              then unirSinRepetidos xs ys 
                              else x : unirSinRepetidos xs ys  
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a] --O(n)
setToList   (S (x:xs)) = agregar x (setToList xs)

agregar :: Eq a => a -> [a] -> [a]
agregar            x    [ ]     = [x]
agregar            x   (y:ys)   = if (x==y) then y:ys else y:agregar x ys   

-- CUAL ES PREFERIBLE??

                        
--3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
--otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
--sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
--por ejemplo). Contrastar la eciencia obtenida en esta implementación con la anterior.
