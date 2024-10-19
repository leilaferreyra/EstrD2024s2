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
poderDePropulsion  (N s) = poderDePropulsionT s 

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
sectorTieneId :: Sector -> SectorId -> Bool
sectorTieneId (S sid1 _ _) sid2 = (sid1 == sid2)

arbolTieneSector :: Tree Sector -> SectorId -> Bool
arbolTieneSector (EmptyT)         _  = False
arbolTieneSector (NodeT s t1 t2) sid = (sectorTieneId s sid)     || 
                                       (arbolTieneSector t1 sid) || 
                                       (arbolTieneSector t2 sid)

naveTieneSector :: Nave -> SectorId -> Bool
naveTieneSector (N t) sid = arbolTieneSector t sid

sectorConComps :: Sector -> [Componente] -> Sector
sectorConComps (S sid scs ts) cs = (S sid (scs ++ cs) ts)

arbolConCompsEn :: Tree Sector -> [Componente] -> SectorId -> Tree Sector
arbolConCompsEn (NodeT s t1 t2) [] _   = (NodeT s t1 t2)
arbolConCompsEn (NodeT s t1 t2) cs sid = if (sectorTieneId s sid)
                                            then (NodeT (sectorConComps s cs) t1 t2)
                                            else (NodeT s (arbolConCompsEn t1 cs sid) (arbolConCompsEn t2 cs sid))  

naveConCompsEn :: Nave -> [Componente] -> SectorId -> Nave
naveConCompsEn (N t) cs sid = (N (arbolConCompsEn t cs sid))

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector [] _ n = n 
agregarASector cs sid n = if (naveTieneSector n sid) 
                             then (naveConCompsEn n cs sid)
                             else n


-- asignarTripulanteA


agregarTrip :: Tripulante -> Sector -> Sector
agregarTrip trip (S sid cs trips) = (S sid cs (trip:trips))

agregarTripAArbolEn :: Tree Sector -> Tripulante -> SectorId -> Tree Sector
agregarTripAArbolEn (EmptyT)          _   _  = (EmptyT)
agregarTripAArbolEn (NodeT s t1 t2) trip sid = if (sectorTieneId s sid) 
                                                    then (NodeT (agregarTrip trip s) t1 t2)
                                                    else (NodeT s (agregarTripAArbolEn t1 trip sid) (agregarTripAArbolEn t1 trip sid))

arbolConTripEnVarios :: Tripulante -> Tree Sector -> [SectorId] -> Tree Sector 
arbolConTripEnVarios    _  arb     []     = arb
arbolConTripEnVarios  trip arb (sid:sids) = arbolConTripEnVarios trip (agregarTripAArbolEn arb trip sid) sids

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--Precond: Todos los id de la lista existen en la nave.
asignarTripulanteA trip ss (N arb) = (N (arbolConTripEnVarios trip arb ss))


-- sectoresAsignados 


sectoresDeNave :: Nave -> Tree Sector
sectoresDeNave (N t) = t

pertenece :: Eq a => a -> [a] -> Bool
pertenece e []      = False
pertenece e (x:xs)  = x == e || pertenece e xs

tripEstaEnSector :: Tripulante -> Sector -> Bool
tripEstaEnSector trip (S _ _ trips) = pertenece trip trips

sectoresDeArbolAsignados :: Tripulante -> Tree Sector -> [SectorId]
sectoresDeArbolAsignados   _        (EmptyT)     = []
sectoresDeArbolAsignados trip (NodeT sect t1 t2) = (singularSi (sectorId sect) (tripEstaEnSector trip sect)) ++
                                                   (sectoresDeArbolAsignados trip t1) ++
                                                   (sectoresDeArbolAsignados trip t2) 

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados trip nave = sectoresDeArbolAsignados trip (sectoresDeNave nave)

singularSi :: a -> Bool -> [a]
singularSi x True  = x:[]
singularSi x False = []

-- tripulantes


tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S _ _ trips) = trips

tripulantesDeArbol :: Tree Sector -> [Tripulante]
tripulantesDeArbol (EmptyT)           = []
tripulantesDeArbol (NodeT sect t1 t2) = tripulantesDeSector sect ++ 
                                        tripulantesDeArbol t1 ++
                                        tripulantesDeArbol t2

tripSinRepetir :: [Tripulante] -> [Tripulante]
tripSinRepetir      []      = []
tripSinRepetir (trip:trips) = if (pertenece trip trips)
                            then tripSinRepetir trips
                            else trip : tripSinRepetir trips

tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripSinRepetir (tripulantesDeArbol t)

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
    
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo |
            Explorador Nombre [Territorio] Lobo Lobo |
            Cria Nombre 
    deriving Show

data Manada = M Lobo
    deriving Show

cria1 = "cria1"
cria2 = "cria2"
cria3 = "cria3"
cria4 = "cria4"
cria5 = "cria5"
cria6 = "cria6"
cria7 = "cria7"
cria8 = "cria8"

territorio1 = "territorio1"
territorio2 = "territorio2"
territorio3 = "territorio3"
territorio4 = "territorio4"

presa1 = "presa1"
presa2 = "presa2"
presa3 = "presa3"
presa4 = "presa4"
presa5 = "presa5"
presa6 = "presa6"
presa7 = "presa7"
presa8 = "presa8"

chayton = "chayton"

galileo = "galileo"

kraven = "kraven"

lope = "lope"

ter1 = "ter1"
ter2 = "ter2"
ter3 = "ter3"

manada2 = M (Explorador chayton [] 
                (Explorador lope [ter1,ter2,ter3]  
                    (Cria cria6) 
                    (Cria cria7)
                )
                (Explorador kraven [ter2]  
                    (Cria cria6) 
                    (Cria cria7)
                )
            )

manada1 = M (Cazador "firulais" ["obejas", "ratones", "raton", "chancho", "chancho", "chancho"] 
               (Explorador "roque" ["riogrande", "riochico"]
                  (Cria "chiquito")
                  (Cria  "rufus"))
               (Explorador "neron" ["rioNahuel"]
                  (Cria "junior")
                  (Cria  "andy"))
               (Cria "rabito")
               )
-- buenaCaza 

{- Otra forma de escribirlo ?

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cazador _ _ l1 l2 l3) = cantidadDeCrias l1 +
                                     cantidadDeCrias l2 + 
                                     cantidadDeCrias l3
cantidadDeCrias (Explorador _ _ l1 l2) = cantidadDeCrias l1 +
                                     cantidadDeCrias l2
cantidadDeCrias           (_)          = 1
-}

--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crías.
cantidadDeCrias :: Lobo -> Int
cantidadDeCrias        (Cria _)        = 1
cantidadDeCrias (Cazador _ _ l1 l2 l3) = cantidadDeCrias l1 +
                                     cantidadDeCrias l2 + 
                                     cantidadDeCrias l3
cantidadDeCrias (Explorador _ _ l1 l2) = cantidadDeCrias l1 +
                                     cantidadDeCrias l2

cantidadDePresas :: Lobo -> Int
cantidadDePresas (Cazador _ cs l1 l2 l3) = length cs + 
                                       cantidadDePresas l1 + 
                                       cantidadDePresas l2 + 
                                       cantidadDePresas l3
cantidadDePresas           _             = 0

buenaCaza :: Manada -> Bool 
buenaCaza (M lobo) = (cantidadDePresas lobo) > (cantidadDeCrias lobo)


-- elAlfa


mayorAcumulacionDePresas :: Lobo -> Int
mayorAcumulacionDePresas (Cazador _ ps l1 l2 l3) = max (length ps) (max 
                                                        (mayorAcumulacionDePresas l1) (max
                                                            (mayorAcumulacionDePresas l2) (mayorAcumulacionDePresas l3)))
mayorAcumulacionDePresas (Explorador _ _ l1 l2)  = max (mayorAcumulacionDePresas l1) (mayorAcumulacionDePresas l2)
mayorAcumulacionDePresas           _           = 0

nombreLobo :: Lobo -> Nombre 
nombreLobo  (Cazador n _ _ _ _) = n 
nombreLobo (Explorador n _ _ _) = n 
nombreLobo   (Cria n)       = n          

loboConPresas :: Lobo -> Int -> Nombre
loboConPresas (Cazador n ps l1 l2 l3) cant = if ((length ps) == cant)
                                                then n ++ ";" 
                                                else "" ++ 
                                                (loboConPresas l1 cant) ++
                                                (loboConPresas l2 cant) ++
                                                (loboConPresas l3 cant) 
loboConPresas (Explorador _ _ l1 l2)  cant = (loboConPresas l1 cant) ++ 
                                             (loboConPresas l2 cant) 
loboConPresas               (_)        _   = ""

buscarAlfaCon :: Lobo -> Int -> Nombre 
buscarAlfaCon lob 0    = (nombreLobo lob)
buscarAlfaCon lob cant = (loboConPresas lob cant)

elAlfa :: Manada -> (Nombre, Int)
--Observacion1 = En caso de haber mas de un alfa devuelve los nombres de todos los alfas separados con un ;
--Observacion2 = En caso de que no haya ningun alfa, devuelve el primer lobo de la manada
elAlfa (M lob) = ((buscarAlfaCon lob (mayorAcumulacionDePresas lob)), (mayorAcumulacionDePresas lob))


-- losQueExploraron


losQueExploraronLobos :: Territorio -> Lobo -> [Nombre]
losQueExploraronLobos  _           (Cria _)         = [] 
losQueExploraronLobos ter (Explorador n ters l1 l2) = (singularSi n (pertenece ter ters)) ++
                                                     (losQueExploraronLobos ter l1) ++
                                                     (losQueExploraronLobos ter l2)
losQueExploraronLobos ter (Cazador _ _ l1 l2 l3)    = (losQueExploraronLobos ter l1) ++
                                                     (losQueExploraronLobos ter l2) ++
                                                     (losQueExploraronLobos ter l3) 

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron ter (M lob) = losQueExploraronLobos ter lob 


-- exploradoresPorTerritorio


tersSinRepetir :: [Territorio] -> [Territorio]
tersSinRepetir []     = []
tersSinRepetir (t:ts) = if (pertenece t ts)
                            then tersSinRepetir ts
                            else t : tersSinRepetir ts

territoriosDe :: Lobo -> [Territorio]
territoriosDe         (Cria _)          = []
territoriosDe  (Cazador _ _ l1 l2 l3)   = territoriosDe l1 ++ territoriosDe l2 ++ territoriosDe l3
territoriosDe (Explorador _ ters l1 l2) = tersSinRepetir (ters ++ territoriosDe l1 ++ territoriosDe l2)

tuplaTerritorioExploradores :: Lobo -> [Territorio] -> [(Territorio, [Nombre])]
tuplaTerritorioExploradores lob []     = []
tuplaTerritorioExploradores lob (t:ts) = (t, (losQueExploraronLobs t lob)) : (tuplaTerritorioExploradores lob ts)

exploradoresPorTerritorioDeLobos :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioDeLobos lob = tuplaTerritorioExploradores lob (territoriosDe lob)

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M lob) = exploradoresPorTerritorioDeLobos lob


-- cazadoresSuperioresDe


manadaEj = M (Cazador "DienteFiloso" ["Búfalos", "Antílopes"]
                (Cria "Hopito")
                (Explorador "Incansable" ["Oeste hasta el río"]
                    (Cria "MechónGris")
                    (Cria "Rabito")
                )
                (Cazador "Garras" ["Antílopes", "Ciervos"]
                    (Explorador "Zarpado" ["Bosque este"]
                        (Cria "Osado")
                        (Cazador "Mandíbulas" ["Cerdos", "Pavos"]
                            (Cria "Desgreñado")
                            (Cria "Malcriado")
                            (Cazador "TrituraHuesos" ["Conejos"]
                                (Cria "Peludo")
                                (Cria "Largo")
                                (Cria "Menudo")
                            )
                        )
                    )
                    (Cria "Garrita")
                    (Cria "Manchas")
                )
            )




cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
--Precondición: hay un lobo con dicho nombre y es unico
cazadoresSuperioresDe n (M lob) = cazadoresSuperioresDeLobo n lob


cazadoresSuperioresDeLobo :: Nombre -> Lobo -> [Nombre]
cazadoresSuperioresDeLobo n1 (Explorador n2 _ l1 l2) = if (n1 == n2)
                                                        then []
                                                        else cazadoresSuperioresDeLobo n1 (loboConSubordinadoEntreDos n1 l1 l2)
cazadoresSuperioresDeLobo n1 (Cazador n2 _ l1 l2 l3) = if (n1 == n2)
                                                        then []
                                                        else n2 : cazadoresSuperioresDeLobo n1 (loboConSubordinadoEntreTres n1 l1 l2 l3)
cazadoresSuperioresDeLobo _  (Cria _)                = []


loboConSubordinadoEntreDos :: Nombre -> Lobo -> Lobo -> Lobo
loboConSubordinadoEntreDos nom lob1 lob2 = if (loboEstaEn nom lob1)
                                            then lob1
                                            else lob2

loboConSubordinadoEntreTres :: Nombre -> Lobo -> Lobo -> Lobo -> Lobo
loboConSubordinadoEntreTres nom lob1 lob2 lob3 = if (loboEstaEn nom lob1)
                                                    then lob1
                                                    else (loboConSubordinadoEntreDos nom lob2 lob3)
                                                 
loboEstaEn :: Nombre -> Lobo -> Bool
loboEstaEn n1 (Cria n2)               = (n1 == n2)
loboEstaEn n1 (Explorador n2 _ l1 l2) = (n1 == n2) || (loboEstaEn n1 l1) || (loboEstaEn n1 l2)
loboEstaEn n1 (Cazador n2 _ l1 l2 l3) = (n1 == n2) || (loboEstaEn n1 l1) || (loboEstaEn n1 l2) || (loboEstaEn n1 l3)