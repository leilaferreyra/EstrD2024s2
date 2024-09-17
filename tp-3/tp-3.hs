--1. Tipos recursivos simples
--1.1. Celdas con bolitas
--     Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo
     deriving Show
data Celda = Bolita Color Celda | CeldaVacia
     deriving Show 

--Casos de ejemplo:
celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda2 = CeldaVacia

--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda         -> Int
nroBolitas    _        CeldaVacia    =  0
nroBolitas    c1       (Bolita c2 c) =  unoSi (esMismoColor c1 c2) +  nroBolitas c1 c

esMismoColor :: Color -> Color -> Bool
esMismoColor    Azul     Azul  =  True
esMismoColor    Rojo     Rojo  =  True
esMismoColor    _        _     =  False 

unoSi :: Bool  -> Int  
unoSi    True  = 1 
unoSi    False = 0

-- Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda           -> Celda
poner    color    (Bolita c celda) = Bolita c (poner color celda)  
poner    color     _               = Bolita color CeldaVacia

--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.
sacar :: Color -> Celda            -> Celda
sacar    _        CeldaVacia        = CeldaVacia
sacar    c1       (Bolita c2 celda) = if esMismoColor c1 c2 
                                       then celda 
                                       else Bolita c2 (sacar c1 celda)

--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN    0      _        celda =  celda
ponerN    n      color    celda =  poner color (ponerN (n-1) color celda)

--1.2. Camino hacia el tesoro
--Tenemos los siguientes tipos de datos
data Objeto = Cacharro | Tesoro
     deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
     deriving Show

--Casos de ejemplo
camino1 = (Nada (Cofre [Cacharro] (Nada (Cofre [Tesoro, Tesoro] Fin))))
camino2 = (Nada (Cofre [Tesoro] (Nada(Cofre [Tesoro] Fin))))
camino3 = (Cofre [Tesoro] Fin)
camino4 = (Nada (Fin))

--Indica si hay un cofre con un tesoro en el camino.
hayTesoro :: Camino     -> Bool
hayTesoro    Fin         = False
hayTesoro    (Cofre o c) = hayTesoro' o || hayTesoro c
hayTesoro    (Nada c)    = hayTesoro c

hayTesoro':: [Objeto] -> Bool
hayTesoro'   [      ]  = False
hayTesoro'   (o:os)    = esTesoro o || hayTesoro' os 

esTesoro:: Objeto -> Bool
esTesoro   Tesoro =  True
esTesoro   _      =  False

--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.

pasosHastaTesoro' :: Camino    -> Int
pasosHastaTesoro'   Fin     = error "No hay tesoros en el camino"
pasosHastaTesoro'   (Cofre o c) = if hayTesoro' o 
                                   then 0 
                                   else 1 + pasosHastaTesoro' c 
pasosHastaTesoro'    (Nada  c)  = 1 + pasosHastaTesoro' c 


--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino     -> Bool
hayTesoroEn    0      (Cofre o _) = hayTesoro' o
hayTesoroEn    n      (Cofre _ c) = hayTesoroEn (n-1) c
hayTesoroEn    n      (Nada c)    = hayTesoroEn (n-1) c
hayTesoroEn    _       Fin        = False

--Indica si hay al menos n tesoros en el camino.
alMenosNTesoros  :: Int -> Camino -> Bool --Issue arreglado 
alMenosNTesoros     n      c = cantTesorosDelCamino c >= n 

cantTesoros :: [Objeto] -> Int
cantTesoros    [] = 0
cantTesoros    (o:os) = unoSi (esTesoro o) + cantTesoros os 

objetosDeCamino :: Camino -> [Objeto] 
objetosDeCamino    (Cofre o c) = o ++ objetosDeCamino c 
objetosDeCamino    (Nada c )     = objetosDeCamino c 
objetosDeCamino    _           = []

cantTesorosDelCamino :: Camino -> Int 
cantTesorosDelCamino    c      = cantTesoros (objetosDeCamino c)


--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
--el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
--incluidos tanto 3 como 5 en el resultado.

cantTesorosEntre :: Int -> Int -> Camino -> Int --Issue arreglado
cantTesorosEntre    _ _ Fin = 0
cantTesorosEntre   i f  (Cofre o c )  = if i<= 1 && f >= 1
                                         then cantTesoros o + cantTesorosEntre (i - 1) (f - 1) c 
                                         else cantTesorosEntre (i - 1) (f - 1) c 
cantTesorosEntre   i f  (Nada c) = cantTesorosEntre (i - 1) (f - 1) c                   

--2. Tipos arbóreos
--2.1. Árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
     deriving Show 

a1::Tree Int 
a1 = NodeT 1 (NodeT 2 EmptyT EmptyT)(NodeT 3 (NodeT 4 EmptyT EmptyT) EmptyT)
a2 :: Tree Int 
a2 = NodeT 3 (EmptyT) (EmptyT)
a3:: Tree Int 
a3 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT) EmptyT)(NodeT 5 (NodeT 6 EmptyT EmptyT) (NodeT 7 (NodeT 9 EmptyT EmptyT) EmptyT))

--1. Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT :: Tree Int     -> Int
sumarT    EmptyT        =  0
sumarT    (NodeT x d i) =  x + sumarT d + sumarT i

--2. Dado un árbol binario devuelve su cantidad de elementos, es decir, el tamaño del árbol (size
--   en inglés).
sizeT :: Tree a       -> Int
sizeT    EmptyT        = 0 
sizeT    (NodeT _ d i) = 1 + sizeT d + sizeT i 

--3. Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT :: Tree Int     -> Tree Int --Issue arreglado
mapDobleT    EmptyT       =  EmptyT
mapDobleT   (NodeT x d i) =  NodeT (x*2) (mapDobleT d) (mapDobleT i)

--4. Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--   árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT     _    EmptyT         = False
perteneceT    n1    (NodeT n2 d i) = n1==n2 || perteneceT n1 d || perteneceT n1 i

--5. Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--   iguales a e.
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT    _     EmptyT         = 0
aparicionesT    n1   (NodeT n2 d i ) = unoSi (n1 == n2) + aparicionesT n1 d + aparicionesT n1 i

--6. Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves :: Tree a                -> [a]
leaves    EmptyT                 = [ ]
leaves   (NodeT x EmptyT EmptyT) = [x]
leaves   (NodeT x d i)           = leaves d ++ leaves i

--7.Dado un árbol devuelve su altura.
heightT :: Tree a      -> Int
heightT    EmptyT       = 0 
heightT   (NodeT x i d) = 1 + max (heightT i) (heightT d)

--8. Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con
--   el derecho, en cada nodo del árbol.
mirrorT :: Tree a     -> Tree a
mirrorT    EmptyT      = EmptyT
mirrorT  (NodeT x i d) = NodeT x (mirrorT d) (mirrorT i) 

--9. Dado un árbol devuelve una lista que representa el resultado de recorrerlo en
--   modo in-order.
toList :: Tree a      -> [a]
toList    EmptyT       = [ ]
toList   (NodeT x i d) = toList i ++ [x] ++ toList d

--10. Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El
--    nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la
--    raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
--    Nota: El primer nivel de un árbol (su raíz) es 0.
levelN :: Int -> Tree a       -> [a]
levelN    n     EmptyT         = [ ]
levelN    0     (NodeT x _ _ ) = [x]
levelN    n     (NodeT _ i d)  = levelN (n-1) i ++ levelN (n-1) d
--11. Dado un árbol devuelve una lista de listas en la que cada elemento representa
--    un nivel de dicho árbol.
listPerLevel :: Tree a         -> [[a]]
listPerLevel    EmptyT          = []
listPerLevel    (NodeT x t1 t2) = [x] : juntarNiveles (listPerLevel t1) (listPerLevel t2)

juntarNiveles :: [[a]] -> [[a]]   -> [[a]]
juntarNiveles    [   ]     yss     =  yss
juntarNiveles     xss     [   ]    =  xss
juntarNiveles    (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

--12. Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a        -> [a]
ramaMasLarga    EmptyT         = [ ]
ramaMasLarga    (NodeT x i d ) = if (heightT i > heightT d)
                                  then x : ramaMasLarga i
                                  else x : ramaMasLarga d

--13. Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz
--    hasta cualquiera de los nodos.
todosLosCaminos :: Tree a         -> [[a]]
todosLosCaminos    EmptyT          = [   ]
todosLosCaminos    (NodeT x t1 t2) = [x] : consACada x (todosLosCaminos t1) ++ consACada x (todosLosCaminos t2)
  
consACada :: a -> [[a]]   -> [[a]]
consACada    x    [   ]    = [   ]
consACada    x    (xs:xss) = (x:xs) : consACada x xss 
--ATENCIÓN: se trata de todos los caminos, y no solamente de los maximales (o
--sea, de la raíz hasta la hoja), o sea, por ejemplo
--todosLosCaminos (NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
--EmptyT)
--(NodeT 4 (NodeT 5 EmptyT EmptyT)
--EmptyT))
-- = [ [1], [1,2], [1,2,3], [1,4], [1,4,5] ]
--OBSERVACIÓN: puede resultar interesante plantear otra función, variación de
--ésta para devolver solamente los caminos maximales.

--2.2. Expresiones Aritméticas
--El tipo algebraico ExpA modela expresiones aritméticas de la siguiente manera:
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA
     deriving Show

--Casos de ejemplo:
x1 = Neg (Sum (Valor 5) (Prod (Valor 5) (Valor 2)))

--1. Dada una expresión aritmética devuelve el resultado evaluarla.
eval :: ExpA       -> Int --Issue arreglado 
eval   (Valor n)    = n
eval   (Sum e1 e2)  = eval e1 + eval e2
eval   (Prod e1 e2) = eval e1 * eval e2
eval   (Neg e)      = - (eval e)

--2. Dada una expresión aritmética, la simplica según los siguientes criterios (descritos utilizando
--   notación matemática convencional):
--a) 0 + x = x + 0 = x
--b) 0 * x = x * 0 = 0
--c) 1 * x = x * 1 = x
--d) - (- x) = x

--Casos de ejemplo:
sim1 = Sum (Valor 0) (Valor 6)
sim2 = Prod (Valor 0) (Valor 6)
sim3 = Prod (Valor 1 ) (Valor 6)
sim4 = Neg (Neg (Valor 6))

simplificar :: ExpA        -> ExpA
simplificar    (Valor n)    = Valor n
simplificar    (Sum e1 e2)  = simplificarSuma (simplificar e1) (simplificar e2) 
simplificar    (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2) 
simplificar    (Neg e)      = simplificarNeg (simplificar e)

simplificarSuma :: ExpA ->   ExpA     -> ExpA 
simplificarSuma    (Valor 0) e         = e
simplificarSuma    e         (Valor 0) = e
simplificarSuma    e1        e2        = Sum e1 e2

simplificarProd :: ExpA ->   ExpA     -> ExpA 
simplificarProd    (Valor 0) e         = Valor 0 
simplificarProd    e         (Valor 0) = Valor 0
simplificarProd    e         (Valor 1) = e
simplificarProd    (Valor 1) e         = e
simplificarProd    e1        e2        = Prod e1 e2 

simplificarNeg :: ExpA   -> ExpA 
simplificarNeg    (Neg e) = e 
simplificarNeg    e       = Neg e