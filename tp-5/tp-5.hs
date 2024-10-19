import SetV1
import QueueV1
import StackV1

ss::Set Int 
ss = addS 5 (addS 6 (addS 7 (emptyS)))

sss::Set Int 
sss = addS 5 (addS 6 (addS 8 (emptyS)))
ssss::Set Int
ssss = addS 5 (addS 6 (addS 9 (emptyS)))
--2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen            [ ]    set   =  [ ]
losQuePertenecen            (x:xs) set   = if belongs x set
                                             then x: losQuePertenecen xs set
                                             else losQuePertenecen xs set 

--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos            [ ] = []
sinRepetidos            xs =  let newSet = listToSet xs
                                in setToList newSet

listToSet :: Eq a => [a] -> Set a
listToSet            [ ] = emptyS
listToSet            (x:xs) = addS x (listToSet xs )

--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
     deriving Show 

tt:: Tree (Set Int)
tt = NodeT ss (NodeT sss (EmptyT) (EmptyT))
              (NodeT ssss (EmptyT) (EmptyT))
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))


--3. Como usuario del tipo abstracto Queue implementar las siguientes funciones:

qq:: Queue Int
qq = enqueue 6 (enqueue 5 (emptyQ))
qqq:: Queue Int
qqq = enqueue 7 (enqueue 8 (emptyQ))
lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
            then 0
            else 1 + lengthQ (dequeue q)


--Cuenta la cantidad de elementos de la cola.
queueToList :: Queue a -> [a]
queueToList    q       =  if (isEmptyQ q)
                           then []
                           else (firstQ q) : queueToList (dequeue q)
           

--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q1
                then q2
                else unionQ (enqueue (firstQ q2) q1) (dequeue q2)

--Inserta todos los elementos de la segunda cola en la primera.

--1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:
apilar :: [a] -> Stack a
apilar    []     = emptySt 
apilar    (x:xs) = push x (apilar xs)
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
st:: Stack Int
st = (push 6(push 5 (emptySt)))

desapilar :: Stack a -> [a]
desapilar    s       =  if isEmptyS s 
                        then []
                        else top s : desapilar (pop s)

--Dada una pila devuelve una lista sin alterar el orden de los elementos.
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos   0       x    s       = push x s 
insertarEnPos   n       x    s       = push x (insertarEnPos (n-1) x (pop s))

--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).



















-- Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
-- del arbol.





