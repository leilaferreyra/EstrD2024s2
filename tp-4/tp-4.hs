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
tesorosDeCofre (Cofre o) = o 

--6. todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _) = [[]]
todosLosCaminos (Bifurcacion _ m1 m2) = (consACada Izq (todosLosCaminos m1)) ++ (consACada Der (todosLosCaminos m2))

consACada :: a -> [[a]]   -> [[a]]
consACada    x    [   ]    = [   ]
consACada    x    (xs:xss) = (x:xs) : consACada x xss 


