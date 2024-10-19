-- Ejercicio 2
-- Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
-- invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
-- desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos
-- del árbol. Justicar por qué la implementación satisface los costos dados.
-- 1. 
import Empresa 
belongsBST :: Ord a => a -> Tree a -> Bool
-- Propósito: dado un BST dice si el elemento pertenece o no al árbol.
-- Costo: O(log N)
belongsBST _ EmptyT = False
belongsBST x (NodeT y ti td) = if (x==y) 
                                then True
                                else if (x < y)
                                 then belongsBST x ti
                                 else belongsBST x td
-- 2. 
insertBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x==y
                            then NodeT y ti td
                            else if (x < y) 
                             then NodeT y (belongsBST x ti) td 
                             else  NodeT y ti (belongsBST x td) 

-- 3. 
deleteBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)
deleteBST _ EmptyT = EmptyT
deleteBST x (NodeT y ti td) = if x == y 
                               then rearmarBST ti td
                               else if x < y
                                then Node x (deleteBST x ti) td 
                                else Node x ti (deleteBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST    EmptyT td = td 
rearmarBST    ti td  = let (m, ti') = splitMaxBST ti
                        in NodeT m ti' td 
        
-- 4. 
splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMinBST    (NodeT x EmptyT td) = (x, td)
splitMinBST   (NodeT x ti td) = let (m,ti') = splitMinBST ti
                                 in (m, NodeT x ti' td)

-- 5. 
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMaxBST     (NodeT x ti EmptyT) = (x, ti)
splitMaxBST    (Node x ti td )      = let (m, td') = splitMaxBST td 
                                        in (m, NodeT x ti td')

-- 6. 
esBST :: Tree a -> Bool
-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N2)
esBST    EmptyT = True
esBST     (NodeT x EmptyT EmptyT) = True
esBST     (NodeT x ti EmptyT) = 
esBST     (NodeT x EmptyT td) = x < td
esBST     (NodeT x ti td) =     esBST ti esBST td 

-- 7. 
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado.
-- Costo: O(log N)
elMaximoMenorA _ EmptyT                 = Nothing
elMaximoMenorA x (NodeT y izq der)      = if 
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST devuelve el máximo elemento que sea menor al elemento dado.
-- Costo: O(log N)
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA n (NodeT x izq der) =
  if n <= x
  then elMaximoMenorA n izq
  else if elMaximoMenorA n der == Nothing
       then Just x
       else elMaximoMenorA n der

-- 8. elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
-- elemento dado.
-- Costo: O(log N)
-- 9. 
balanceado :: Tree a -> Bool
-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
-- nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N2)


elMax:: Ord a => Tree a -> a
--El arbol no puede ser vacio
elMax  (NodeT x ti EmptyT) = x 
elMax  (NodeT x ti td)     = elMax td 

 13 
      12
     /   \
    7     20
   / \      \
  5   8      21
 / \   \     / \
2   6   10  19  22
 \     / \       \
  3   9   11      23

-- Como usuario del tipo Empresa implementar las siguientes operaciones, calculando el costo obtenido al implementarlas, y justicando cada uno adecuadamente.
-- 
comenzarCon :: [SectorId] -> [CUIL] -> Empresa
-- Propósito: construye una empresa con la información de empleados dada. Los sectores no
-- tienen empleados.
-- Costo: calcular.
comenzarCon ss es =  agregarEmpleados es (empresaConSectores ss(consEmpresa))

agregarEmpleados :: [CUIL] -> Empresa -> Empresa 
agregarEmpleados [] emp     = emp
agregarEmpleados (x:xs) emp = agregarEmpleados xs (agregarEmpleado [] x empresa)

empresaConSectores :: [SectorId] -> Empresa -> Empresa
empresaConSectores  []   emp  = emp
empresaConSectores (x:xs) emp = empresaConSectores xs (agregarSector x emp)
-- 
recorteDePersonal :: Empresa -> Empresa
-- Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
-- Costo: calcular.
recorteDePersonal emp = let mitadEmp = div (length (todosLosCUIL emp)) 2 
                          in recortarCantidadDePersonal (todosLosCUIL emp) mitadEmp emp 

recortarCantidadDePersonal :: [CUIL]-> Int -> Empresa -> Empresa
recortarCantidadDePersonal  _ 0 emp = emp 
recortarCantidadDePersonal  (x:xs) n emp = recortarCantidadDePersonal xs (n-1) (borrarEmpleado x emp)
-- 
convertirEnComodin :: CUIL -> Empresa -> Empresa
-- Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
-- Costo: calcular. 
convertirEnComodin cuil empresa = agregarEmpleado (todosLosSectores empresa) cuil empresa 

-- 
esComodin :: CUIL -> Empresa -> Bool
--Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
--Costo: calcular.  --O(S) ??
esComodin cuil emp=  let sectoresEmpleado = setToList (sectores (buscarPorCUIL cuil emp))
                          in  length (todosLosSectores empresa) == length sectoresEmpleado
