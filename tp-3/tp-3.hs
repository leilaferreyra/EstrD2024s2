--1. Tipos recursivos simples
--1.1. Celdas con bolitas
--     Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo
     deriving Show
data Celda = Bolita Color Celda | CeldaVacia
     deriving Show 

-- En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
-- de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
--ser la siguiente:
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda2= CeldaVacia
--Implementar las siguientes funciones sobre celdas:
--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas    _        CeldaVacia = 0
nroBolitas    c1        (Bolita c2 c)    =   unoSi (esMismoColor c1 c2) +  nroBolitas c1 c

esMismoColor :: Color -> Color -> Bool
esMismoColor     Azul    Azul  =  True
esMismoColor     Rojo    Rojo  =  True
esMismoColor     _       _     =  False 

unoSi :: Bool -> Int  
unoSi    True = 1 
unoSi    False= 0
-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner color CeldaVacia = Bolita color CeldaVacia
poner color (Bolita c celda) = Bolita c (poner color celda)  

esCeldaVacia :: Celda -> Bool
esCeldaVacia    CeldaVacia = True
esCeldaVacia    _          = False
--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.
sacar :: Color -> Celda           -> Celda
sacar    _        CeldaVacia       = CeldaVacia
sacar    c1    (Bolita c2 celda) = if esMismoColor c1 c2 
                                     then celda 
                                     else Bolita c2 (sacar c1 celda)

--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN    0      _        celda = celda
ponerN    n      color    celda = poner color (ponerN (n-1) color celda)

--1.2. Camino hacia el tesoro
--Tenemos los siguientes tipos de datos
data Objeto = Cacharro | Tesoro
     deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
     deriving Show

camino1 = (Nada (Cofre [Cacharro] (Nada (Cofre [Tesoro] Fin))))
camino2 = (Nada (Cofre [Tesoro] (Nada(Cofre [Tesoro] Fin))))
camino3 = (Cofre [Tesoro] Fin)
camino4 = (Nada (Fin))
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre o c) = hayTesoro' o || hayTesoro c
hayTesoro (Nada c) = hayTesoro c

hayTesoro':: [Objeto] -> Bool
hayTesoro' []  = False
hayTesoro' (o:os) = esTesoro o || hayTesoro' os 

esTesoro:: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False
--Indica si hay un cofre con un tesoro en el camino.
pasosHastaTesoro :: Camino -> Int 
pasosHastaTesoro    c      =  if (hayTesoro c)
                              then pasosHastaTesoro' c 
                              else error "No hay tesoros en el camino"

pasosHastaTesoro' :: Camino -> Int
pasosHastaTesoro'   (Cofre o c) = if hayTesoro' o 
                                  then 0 
                                  else 1 + (pasosHastaTesoro' c )
pasosHastaTesoro'    (Nada  c)   = 1 + pasosHastaTesoro' c 


--Me deberia asegurar la precondicion con un mensaje de error personalizado? 

--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 (Cofre o _) = hayTesoro' o
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n-1) c
hayTesoroEn n (Nada c) = hayTesoroEn (n-1) c
hayTesoroEn _ Fin = False

--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
alMenosNTesoros :: Int -> Camino     -> Bool
alMenosNTesoros    0      _           = True 
alMenosNTesoros    n      (Cofre o c) = hayTesoro' o && alMenosNTesoros (n-1) c
alMenosNTesoros    n      (Nada c)    = alMenosNTesoros (n-1) c
alMenosNTesoros    n      Fin         = False 
--Indica si hay al menos n tesoros en el camino.
--(desafío) 
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n1 n2 camino = if  n1 > n2 
                              then 0
                              else unoSi (hayTesoroEn n1 camino) + cantTesorosEntre (n1 + 1) n2 camino
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.

--2. Tipos arbóreos
--2.1. Árboles binarios
--Dada esta denición para árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
     deriving Show 
--dena las siguientes funciones utilizando recursión estructural según corresponda:
--1.
a1::Tree Int 
a1 = NodeT 1 (NodeT 2 EmptyT EmptyT)(NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT)
a2 :: Tree Int 
a2 = NodeT 3 (EmptyT) (EmptyT)
a3:: Tree Int 
a3 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT)(NodeT 5 (NodeT 6 EmptyT EmptyT) (NodeT 7 (NodeT 9 EmptyT EmptyT) EmptyT))
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x d i) = x + sumarT d + sumarT i

--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
--2. 
sizeT :: Tree a -> Int
sizeT    EmptyT = 0 
sizeT    (NodeT _ d i) = 1 + sizeT d + sizeT i 
--Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--en inglés).
--3. 
mapDobleT :: Tree Int -> Tree Int
mapDobleT    EmptyT  = EmptyT
mapDobleT    (NodeT x d i) = (NodeT (x+1)) (mapDobleT d) (mapDobleT i)
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
--4. 
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT    _     EmptyT = False
perteneceT    n1    (NodeT n2 d i) = (n1==n2) || (perteneceT n1 d) || (perteneceT n1 i)
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
--5. 
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT    _   EmptyT = 0
aparicionesT    n1   (NodeT n2 d i ) = unoSi (n1 == n2) + (aparicionesT n1 d) + (aparicionesT n1 i)
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
--6. 
leaves :: Tree a -> [a]
leaves    EmptyT = []
leaves    (NodeT x EmptyT EmptyT) = [x]
leaves    (NodeT x d i) = (leaves d) ++ (leaves i)
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
--NOTA: en este tipo se dene como hoja a un nodo con dos hijos vacíos.

--7.
heightT :: Tree a -> Int
heightT EmptyT = 0 
heightT (NodeT x i d) =  1 + max (heightT i) (heightT d)

--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es
--la cantidad de niveles del árbol1
--La altura para EmptyT es 0, y para una hoja
--es 1.
--8.
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x i d) = NodeT x (mirrorT d) (mirrorT i) 
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con
--el derecho, en cada nodo del árbol.
--9. 
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x i d) = toList i ++ [x] ++ toList d
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en
--modo in-order.
--Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo,
--luego la raiz y luego los elementos del hijo derecho.
--10. 
levelN :: Int -> Tree a -> [a]
levelN    n      EmptyT = []
levelN    0      (NodeT x _ _ ) = [x]
levelN     n      (NodeT _ i d) =  levelN (n-1) i ++ levelN (n-1) d
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El
--nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la
--raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
--Nota: El primer nivel de un árbol (su raíz) es 0.
--11.
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       yss      = yss
juntarNiveles xss      []       = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss


--Dado un árbol devuelve una lista de listas en la que cada elemento representa
--un nivel de dicho árbol.
--12. 
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x i d ) =  if (heightT i > heightT d)
                              then x : ramaMasLarga i
                              else x : ramaMasLarga d
--Devuelve los elementos de la rama más larga del árbol
--13. 
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss 
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz
--hasta cualquiera de los nodos.
--ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o
--sea, de la raíz hasta la hoja), o sea, por ejemplo
--todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
--EmptyT)
--(NodeT 4 (NodeT 5 EmptyT EmptyT)
--EmptyT))
-- = [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
--OBSERVACIÓN: puede resultar interesante plantear otra función, variación de
--ésta para devolver solamente los caminos maximales.