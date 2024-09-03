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

--colorDeBolita :: Celda -> Color 
--colorDeBolita (CeldaVacia)     = error "Una celda vacia no tiene color"
--colorDeBolita (Bolita color _) = color

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
sacar    color    (Bolita c celda) = if esMismoColor color c 
                                     then celda 
                                     else Bolita c (sacar color celda)

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
a1 = NodeT 1 (NodeT 3 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)
a2 :: Tree Int 
a2 = NodeT 3 (EmptyT) (EmptyT)
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