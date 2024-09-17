data Pizza = Prepizza  | Capa Ingrediente Pizza 
  deriving Show 

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int 
  deriving Show 
unoSi :: Bool -> Int
unoSi    True = 1
unoSi    False = 0 

--Dada una lista devuelve la cantidad de ingredientes.
cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing p ) = 1 + cantidadDeCapas p

pizza2 = Capa Queso (Capa Salsa Prepizza)
pizza3 = Capa (Aceitunas 8) 
              (Capa Jamon (Capa Salsa Prepizza))
--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza     []            = Prepizza
armarPizza     (i:is)        = Capa i (armarPizza is )

--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing p) = if esIng Jamon ing 
                           then sacarJamon p 
                           else Capa ing (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon    Jamon       = True
esJamon    _           = False 

--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing p) = esQuesoOesSalsa ing && (tieneSoloSalsaYQueso p )

esQuesoOesSalsa :: Ingrediente -> Bool
esQuesoOesSalsa    i           = ( esIng Queso i )|| (esIng Salsa i )

esIng :: Ingrediente -> Ingrediente -> Bool
esIng    Jamon          Jamon        = True
esIng    Queso          Queso        = True
esIng    Jamon          Jamon        = True
esIng    Salsa          Salsa        = True 
esIng    (Aceitunas _)  (Aceitunas _) = True 
esIng    _               _            = False 

--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas    Prepizza = Prepizza
duplicarAceitunas    (Capa ing p) = Capa (duplicarAceitunasDeIng ing) (duplicarAceitunas p)

duplicarAceitunasDeIng :: Ingrediente -> Ingrediente
duplicarAceitunasDeIng (Aceitunas n) = (Aceitunas (n*2))
duplicarAceitunasDeIng  i            = i  

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza    [] = []
cantCapasPorPizza     (p:ps) = ((cantidadDeCapas p), p) : (cantCapasPorPizza ps)

data Dir = Izq | Der
 deriving Show
data Objeto = Tesoro | Chatarra
   deriving Show
data Cofre = Cofre [Objeto]
   deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
   deriving Show

m1 = (Fin (Cofre ([Tesoro,Chatarra])))
m2 = (Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [Chatarra])) (Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [Tesoro])) (Fin (Cofre [Chatarra]))))
--1. Indica si hay un tesoro en alguna parte del mapa.
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) =  hayTesoroEnCofre c || (hayTesoro m1) || (hayTesoro m2) 

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre    (Cofre o) = hayTesoroEnLista o 

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista   [      ]  = False
hayTesoroEnLista   (o:os)    = esTesoro o || hayTesoroEnLista os 

esTesoro:: Objeto -> Bool
esTesoro   Tesoro =  True
esTesoro   _      =  False

--2.
--Indica si al nal del camino hay un tesoro. Nota: el nal de un camino se representa con una
--lista vacía de direcciones

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn    [   ]    m = hayTesoroAqui m
hayTesoroEn    (d:ds)   m = hayTesoroEn ds (siguienteDirEn d m)

hayTesoroAqui :: Mapa -> Bool
hayTesoroAqui    (Fin c) = hayTesoroEnCofre c 
hayTesoroAqui    (Bifurcacion c m1 m2) = hayTesoroEnCofre c 

siguienteDirEn :: Dir -> Mapa -> Mapa
siguienteDirEn  _  (Fin _)  = error "Ya llegaste al final"
siguienteDirEn  Izq (Bifurcacion _ m1 _) = m1 
siguienteDirEn  Der (Bifurcacion _ _ m2) = m2 

--3. 
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.

caminoAlTesoro :: Mapa -> [Dir] --RARI VERRR

caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c
                                       then []
                                       else if hayTesoro m1
                                        then Izq : caminoAlTesoro m1 
                                         else Der : caminoAlTesoro m2 


--Indica el camino de la rama más larga. RARI VERRR
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga    (Fin c) = []
caminoDeLaRamaMasLarga    (Bifurcacion c m1 m2) = if heightT m1 > heightT m2
                                                   then Izq : caminoDeLaRamaMasLarga m1 
                                                   else Der : caminoDeLaRamaMasLarga m2 


heightT ::  Mapa  -> Int
heightT    (Fin _)       = 0 
heightT   (Bifurcacion c m1 m2) = 1 + max (heightT m1) (heightT m2)

ramaMasLarga :: Tree a        -> [a]
ramaMasLarga    EmptyT         = [ ]
ramaMasLarga   (NodeT x i d)   = x: laMasLarga (ramaMasLarga i) (ramaMasLarga d)

laMasLarga :: [a] -> [a] -> [a]
laMasLarga     a      b  = if length a > length b 
                              then a 
                              else b 


--5. tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel   (Fin c)   = tesorosDeCofre c : []
tesorosPorNivel    (Bifurcacion c m1 m2 ) = tesorosDeCofre c : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)

juntarNiveles :: [[a]] -> [[a]]   -> [[a]]
juntarNiveles    [   ]     yss     =  yss
juntarNiveles     xss     [   ]    =  xss
juntarNiveles    (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

tesorosDeCofre :: Cofre -> [Objeto]
tesorosDeCofre (Cofre o) = tesorosDeLista o 

tesorosDeLista :: [Objeto] -> [Objeto] 
tesorosDeLista []     = []
tesorosDeLista (o:os) = if (esTesoro o)
                            then o : tesorosDeLista os
                            else tesorosDeLista os
--6. todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ m1 m2) = (consACada Izq (todosLosCaminos m1)) ++ (consACada Der (todosLosCaminos m2))

consACada :: a -> [[a]]   -> [[a]]
consACada    x    [   ]    = [   ]
consACada    x    (xs:xss) = (x:xs) : consACada x xss 

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
 deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
 deriving Show 
data Sector = S SectorId [Componente] [Tripulante]
 deriving Show 
type SectorId = String 
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
 deriving Show 
data Nave = N (Tree Sector)
 deriving Show 

nave = N (NodeT (S "Nave1" [LanzaTorpedos] ["Juan", "Juana"])
                (NodeT (S "Nave2" [LanzaTorpedos] ["Juan", "Juana"])
                     EmptyT 
                     EmptyT)
                (EmptyT))
--modelaremos una Nave como un tipo algebraico, el cual nos permite construir una nave espacial,
--dividida en sectores, a los cuales podemos asignar tripulantes y componentes. La representación
--es la siguiente:

--1. 
--Propósito: Devuelve todos los sectores de la nave.
sectores :: Nave -> [SectorId]
sectores   (N s)  = sectoresDeTree s 

sectoresDeTree :: Tree Sector -> [SectorId]
sectoresDeTree    EmptyT    = []
sectoresDeTree    (NodeT s1 t1 t2) = (sectorId s1) : sectoresDeTree t1 ++ sectoresDeTree t2 

sectorId :: Sector -> SectorId 
sectorId (S sec _ _) = sec 

--2. 
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion  (N s) = poderDePropulsionTree s 

poderDePropulsionT :: Tree Sector -> Int
poderDePropulsionT    EmptyT = 0 
poderDePropulsionT   (NodeT s t1 t2 ) =  poderDePropulsionDeSector s + poderDePropulsionT t1 + poderDePropulsionT t2

poderDePropulsionDeSector :: Sector -> Int 
poderDePropulsionDeSector    (S _ c t ) = poderDePropulsionComponentes c 


poderDePropulsionComponentes :: [Componente] -> Int 
poderDePropulsionComponentes    [] = 0
poderDePropulsionComponentes    (c:cs) = (poder c) + poderDePropulsionComponentes cs  

poder :: Componente -> Int
poder    (Motor n) = n
poder     _        = 0   

--3. 
--Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles    (N s) = barrilesDeSectorT s

barrilesDeSectorT :: Tree Sector -> [Barril]
barrilesDeSectorT    EmptyT = []
barrilesDeSectorT    (NodeT s t1 t2) = (barrilesDeSector s) ++ barrilesDeSectorT t1 ++ barrilesDeSectorT t2 

barrilesDeSector :: Sector -> [Barril]
barrilesDeSector   (S _ c t) = barrilesDeComponentes c 

barrilesDeComponentes :: [Componente] -> [Barril]
barrilesDeComponentes    [] = []
barrilesDeComponentes    (c:cs) = barrilesDeComponente c ++ barrilesDeComponentes cs 

barrilesDeComponente :: Componente -> [Barril]
barrilesDeComponente     (Almacen bs) = bs
barrilesDeComponente     _            = []

--4. 
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector    os              id          (N s)   = N (agregarASectorT os id s)

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorT    _              _            EmptyT        = EmptyT
agregarASectorT   os              id           (NodeT s t1 t2) = if esSector id s 
                                                                  then (NodeT (agregarComponentesASector os s ) t1 t2)
                                                                  else (NodeT s (agregarASectorT t1) (agregarASectorT t2))


esSector :: SectorId -> Sector -> Bool
esSector    id1          (S id2 _ _) = id1 == id2


agregarComponentesASector :: [Componente] -> Sector -> Sector 
agregarComponentesASector cs  (S id c t) = (S id (c ++ cs) t )
