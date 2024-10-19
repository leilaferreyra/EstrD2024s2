
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

--d) Dado un par de números devuelve el mayor de estos.
maxDelPar :: (Int,Int) -> Int
maxDelPar (n,m) = if n > m  
                  then n 
                  else m

--  2)
-- ejemplos: 
-- maxDelPar (sucesor 3, (sumar 3 5))
-- maxDelPar (divisionYResto 6 7)
-- sucesor (sumar 10 20)
-- sumar (maxDelPar(3,4)) (maxDelPar(5,6))

--3) TIPOS ENUMERATIVOS
-- 1)
--a) Dada una dirección devuelve su opuesta.

data Dir = Norte | Este | Sur | Oeste
     deriving Show
 
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur   = Norte
opuesto Este  = Oeste
opuesto Oeste = Este 
--b)Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.

iguales :: Dir -> Dir -> Bool
iguales    Norte  Norte  = True
iguales    Sur    Sur    = True
iguales    Este   Este   = True
iguales    Oeste  Oeste  = True 
iguales    _      _      = False

--c)Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
--  la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
--  total o parcial? ¾Por qué?

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este  = Sur
siguiente Sur   = Oeste
siguiente Oeste = error "No existe una siguiente direccion"
-- PRECONDICION: No se puede dar la siguiente direccion de opuesto. La funcion es parcial.

--  2) Denir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
--     Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
--     es domingo. Luego implementar las siguientes funciones:

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
     deriving Show

--a) Devuelve un par donde la primera componente es el primer día de la semana, y la
--   segunda componente es el último día de la semana. Considerar denir subtareas útiles
--   que puedan servir después

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia =  (primerDiaDeSemana,ultimoDiaDeSemana)

primerDiaDeSemana :: DiaDeSemana
primerDiaDeSemana =  Lunes

ultimoDiaDeSemana :: DiaDeSemana
ultimoDiaDeSemana =  Domingo

--b) Dado un día de la semana indica si comienza con la letra M.

empiezaConM :: DiaDeSemana -> Bool
empiezaConM    Martes      =  True
empiezaConM    Miercoles   =  True
empiezaConM    _           =  False

--c) Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
--   la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
--   analizados en esta y cualquier subtarea, deberían ser no más de 9 casos)
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues    d1             d2          =  numeroDia d1 > numeroDia d2

numeroDia :: DiaDeSemana -> Int 
numeroDia    Lunes       =  1 
numeroDia    Martes      =  2 
numeroDia    Miercoles   =  3 
numeroDia    Jueves      =  4 
numeroDia    Viernes     =  5
numeroDia    Sabado      =  6 
numeroDia    Domingo     =  7

--d) Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio    Lunes       =  False
estaEnElMedio    Domingo     =  False
estaEnElMedio    _           =  True 

--4) REGISTROS
--  1) Denir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
--     siguientes funciones:

data Persona = P String Int 
              -- Nombre Edad   
     deriving Show

--Devuelve el nombre de una persona
nombre :: Persona -> String
nombre    (P n _) =  n
--Devuelve la edad de una persona
edad :: Persona -> Int
edad    (P _ e ) = e

--Aumenta en uno la edad de la persona.
crecer :: Persona -> Persona
crecer    (P n e) =  P n (sucesor e)

--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre    n         (P _ e) =  P n e

--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra    p1         p2      = (edad p1) > (edad p2)

--Dadas dos personas devuelve a la persona que sea mayor
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor    p1         p2      =  if (esMayorQueLaOtra p1 p2) 
                                      then p1
                                      else p2

--  2) Denir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
--     porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego denir las
--     siguientes funciones:
data Pokemon = PK TipoDePokemon Int
      deriving Show

data TipoDePokemon = Agua | Fuego | Planta 
       deriving Show

data Entrenador = En String Pokemon Pokemon
      deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA    pk1        pk2     =  mejorTipo (tipo pk1) (tipo pk2)

tipo:: Pokemon -> TipoDePokemon
tipo  (PK t _ ) = t 

mejorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mejorTipo    Agua             Fuego         =  True
mejorTipo    Fuego            Planta        =  True
mejorTipo    Planta           Agua          =  True
mejorTipo    _                _             =  False

--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe t e = unoSiCeroSiNo(sonElMismoTipo t (tipo (primerPKEntrenador e))) +
                          unoSiCeroSiNo(sonElMismoTipo t (tipo (segundoPKEntrenador e)))

sonElMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
sonElMismoTipo    Fuego            Fuego         =  True
sonElMismoTipo    Planta           Planta        =  True
sonElMismoTipo    Agua             Agua          =  True 
sonElMismoTipo    _                _             =  False 

unoSiCeroSiNo::Bool -> Int
unoSiCeroSiNo  True  = 1
unoSiCeroSiNo  False = 0

primerPKEntrenador :: Entrenador -> Pokemon
primerPKEntrenador    (En _ pk _) =  pk

segundoPKEntrenador :: Entrenador -> Pokemon
segundoPKEntrenador    (En _ _ pk) = pk

-- Junta los Pokémon de dos entrenadores en una lista
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon    (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2 

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (En _ pk1 pk2) = [pk1,pk2]

--3. Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int]  -> [Int] 
sucesores    (x:xs) =  x + 1 : sucesores xs 
sucesores _ = []

--6. Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]]  -> [a] --ISSUE ARREGLADO
aplanar    (xs:xss) =  xs ++ aplanar xss
aplanar    _      =  []

--7. Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece :: Eq a => a -> [a]    -> Bool
pertenece            _    [ ]    =  False   
pertenece            e    (x:xs) =  e == x || pertenece e xs

--8. Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones :: Eq a => a -> [a]   -> Int
apariciones            _    [ ]    =  0
apariciones            e    (x:xs) = unoSi (e==x) + apariciones e xs

--9. Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA :: Int -> [Int]  -> [Int]
losMenoresA    _      [   ]  =  [   ] 
losMenoresA    n      (x:xs) =  if x < n
                                 then   x : losMenoresA n xs
                                 else   losMenoresA n xs 

--10. Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]]  -> [[a]]
lasDeLongitudMayorA    _      [   ]  =  [   ]
lasDeLongitudMayorA    n      (x:xs) =  if (longitud x) > n 
                                         then x : lasDeLongitudMayorA n xs 
                                         else lasDeLongitudMayorA n xs 

--13. Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Denida en Haskell como reverse.
invertir :: [a]    -> [a]
invertir    [ ]    =  [ ]
invertir    (x:xs) =  invertir xs ++ [x]

--14. Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--    máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--    las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int]  -> [Int] --ISSUE ARREGLADO
zipMaximos     xs      [   ]  =  xs
zipMaximos     [  ]    ys      =  ys
zipMaximos     (x:xs)  (y:ys) =  if x > y 
                                  then x : zipMaximos xs ys 
                                  else y : zipMaximos xs ys
    
--15. Dada una lista devuelve el mínimo
elMinimo :: Ord a => [a]    -> a
elMinimo             [ ]    =  error "La lista está vacía"
elMinimo             (x:[]) =  x
elMinimo             (x:xs) =  if x < elMinimo xs 
                                then x 
                                else elMinimo xs
--2. RECURSION SOBRE NUMEROS 

--1. Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial    0   =  1 
factorial    n   =  n * factorial (n-1) 

--2. Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--   n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int] --ISSUE ARREGLADO
cuentaRegresiva    n   = if n < 1
                          then []
                          else n : cuentaRegresiva (n-1)

--3. Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir    0      _ =  [ ]
repetir    n      e =  e : repetir (n-1) e

                           
--REGISTROS
--1. Denir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
--siguientes funciones:
data Persona = P String Int 
              -- Nombre Edad   
     deriving Show


edad :: Persona -> Int
edad    (P _ e) =  e 

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA    _      [       ] =  [       ]
mayoresA    n      (x:xs)    =  if edad x > n
                                 then x : mayoresA n xs
                                 else mayoresA n xs 

--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
promedioEdad :: [Persona] -> Int
promedioEdad    [       ] =  error "No se puede hacer el promedio de una lista vacia"
promedioEdad    xs        =  div (sumatoria (edades xs)) (longitud xs)

edades :: [Persona] -> [Int]
edades    [       ] =  [  ]
edades    (x:xs)    =  (edad x) : edades xs 

--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
elMasViejo :: [Persona] -> Persona
elMasViejo    [       ] =  error"No hay personas en la lista"
elMasViejo    (x:[])    = x 
elMasViejo    (x:xs)    = if (edad x) > edad (elMasViejo xs)
                           then x 
                           else elMasViejo xs 

---Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
--lista al menos posee una persona.

data TipoDePokemon = Fuego | Agua | Planta 
     deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int 
     deriving Show

data Entrenador = ConsEntrenador String [Pokemon] 
     deriving Show

--De
--Devuelve la cantidad de Pokémon de determinado tipo    que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe    t                e          =  cantPokemonDeTipo t (pokemonesDe e)

cantPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Int --ISSUE ARREGLADO (NOMBRE)
cantPokemonDeTipo    _                [       ] =  0 
cantPokemonDeTipo    t                (p:ps)    =  unoSi (esMismoTipo t (tipo p)) + cantPokemonDeTipo t ps
                       
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
--a los Pokemon del segundo entrenador.
tipo :: Pokemon           -> TipoDePokemon
tipo    (ConsPokemon t _) =  t

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_    t e1 e2 = contarVencedores t (pokemonesDe e1) (pokemonesDe e2)

contarVencedores:: TipoDePokemon ->  [Pokemon] -> [Pokemon] -> Int
contarVencedores    _                [       ]    _          = 0
contarVencedores    t                (p1:ps1)     ps2        = (unoSi ((esDeTipo p1 t) && (superaATodos p1 ps2))) 
                                                               + contarVencedores t ps1 ps2 

esDeTipo :: Pokemon -> TipoDePokemon -> Bool
esDeTipo    p1         t              =  esMismoTipo (tipo p1) t 

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo    Agua             Agua          = True
esMismoTipo    Planta           Planta        = True
esMismoTipo    Fuego            Fuego         = True
esMismoTipo    _                _             = False 

superaATodos:: Pokemon -> [Pokemon] -> Bool
superaATodos   _          [       ] =  True 
superaATodos   pk1        (pk2:ps)  =  superaA pk1 pk2 && superaATodos pk1 ps 

superaA :: Pokemon -> Pokemon -> Bool
superaA    pk1        pk2     =  mejorTipo (tipo pk1) (tipo pk2)

mejorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mejorTipo    Agua             Fuego         =  True
mejorTipo    Fuego            Planta        =  True
mejorTipo    Planta           Agua          =  True
mejorTipo    _                _             =  False

--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon    e          =  hayUnoDeCada (pokemonesDe e)

hayUnoDeCada :: [Pokemon] -> Bool -- ARREGLADO ISSUE DE SUBTAREAS
hayUnoDeCada    [       ] =  False 
hayUnoDeCada    ps        =  hayPokemonDeTipo Agua ps && hayPokemonDeTipo Fuego ps && hayPokemonDeTipo Planta ps

hayPokemonDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonDeTipo     _               [       ] =  False 
hayPokemonDeTipo     t               (p:ps)    = esMismoTipo (tipo p) t || hayPokemonDeTipo t ps

--3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
--de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
--una lista de personas con diferente rol. La denición es la siguiente:
data Seniority = Junior | SemiSenior | Senior 
     deriving Show
data Proyecto = ConsProyecto String 
     deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto   
     deriving Show 
data Empresa = ConsEmpresa [Rol] 
     deriving Show 

--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa ->             [Proyecto]
proyectos    e = proyectos' (rolesDe e )
 
proyectos' :: [Rol]  -> [Proyecto]
proyectos'    [   ]  =  [        ]
proyectos'    (r:rs) = agregarSiNoExiste (proyecto r) (proyectos' rs) 

agregarSiNoExiste :: Proyecto -> [Proyecto] -> [Proyecto]
agregarSiNoExiste    p           [        ] =  [p]
agregarSiNoExiste    p           (p2:ps)    =  if esMismoProyecto p p2 
                                                then p2:ps 
                                                else p2:agregarSiNoExiste p ps 

esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto    (ConsProyecto p1) (ConsProyecto p2) = p1 == p2

proyecto :: Rol               -> Proyecto 
proyecto    (Developer _ p)   = p
proyecto    (Management _ p)  = p
                     
--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
--además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior    e          ps          = losDevSenior' (rolesDe e) ps 

losDevSenior':: [Rol] -> [Proyecto] -> Int 
losDevSenior' []    _     =  0 
losDevSenior' (r:rs) ps = unoSi (esDevSenior r && perteneceAAlgunProyecto r ps) + losDevSenior' rs ps

rolesDe :: Empresa        -> [Rol]
rolesDe   (ConsEmpresa rs) = rs

perteneceAAlgunProyecto :: Rol -> [Proyecto] -> Bool
perteneceAAlgunProyecto    _      [        ] = False 
perteneceAAlgunProyecto    r      (p:ps)     = esMismoProyecto (proyecto r) p || perteneceAAlgunProyecto r ps

esDevSenior :: Rol -> Bool  --ISSUE ARREGLADO
esDevSenior    r   =  (esDev r) && esSenior r 
esDevSenior    _   = False 

esDev :: Rol            -> Bool 
esDev   (Developer _ _) = True
esDev   _               = False 

esSenior:: Rol -> Bool 
esSenior   (Developer Senior _ )  = True
esSenior   (Management Senior _ ) = True
esSenior   _                      = False 

--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa   -> Int
cantQueTrabajanEn    ps            e         =  cantRolQueTrabajanEn ps (rolesDe e)

cantRolQueTrabajanEn :: [Proyecto] -> [Rol]  -> Int 
cantRolQueTrabajanEn    [        ]    _      =  0
cantRolQueTrabajanEn    (p:ps)        rs     =  cantDeRolEnProyecto p rs + cantRolQueTrabajanEn ps rs

cantDeRolEnProyecto :: Proyecto -> [Rol]  -> Int
cantDeRolEnProyecto    p           [   ]  =  0
cantDeRolEnProyecto    p           (r:rs) =  unoSi (esMismoProyecto p (proyecto r)) + cantDeRolEnProyecto p rs

--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
--cantidad de personas involucradas.  --ISSUE ARREGLADO
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto    e       =  asignadosPorProyecto' (rolesDe e)

asignadosPorProyecto' :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyecto'    [   ]  = [               ]
asignadosPorProyecto'    (r:rs) = cantidadPorProyecto (proyecto r) (asignadosPorProyecto' rs)

cantidadPorProyecto  :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
cantidadPorProyecto     p           [               ]  = [(p, 1)]
cantidadPorProyecto     p1          ((p2, n):ps)       = if  esMismoProyecto p1 p2
                                                      then (p2, n + 1) : ps
                                                      else  (p2, n) : cantidadPorProyecto p1 ps
                              
proyecto :: Rol               -> Proyecto 
proyecto    (Developer _ p)   = p
proyecto    (Management _ p)  = p

rolesDe :: Empresa        -> [Rol]
rolesDe   (ConsEmpresa rs) = rs

esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto    (ConsProyecto p1) (ConsProyecto p2) = p1 == p2

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

module Empresa 
import MapV1
import SetV1 
   (ConsE, constEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores,agregarSector,agregarEmpleado)
   where 
type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)

data Empleado = E Int (Set SectorId)
                 --CUIL --SECTORES A LOS QUE PERTENECE.
consEmpleado :: CUIL -> Empleado --O(1)
-- Propósito: construye un empleado con dicho CUIL.
-- Costo: O(1)
consEmpleado c = E c emptyS

-- 
cuil :: Empleado -> CUIL
-- Propósito: indica el CUIL de un empleado.
-- Costo: O(1)
cuil (E c _) = c 
-- 
incorporarSector :: SectorId -> Empleado -> Empleado
-- Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
-- Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector s (S c ss) = S c (addS s ss)
-- 
sectores :: Empleado -> [SectorId]
-- Propósito: indica los sectores en los que el empleado trabaja.
-- Costo: O(S)
sectores (E _ ss) = setToList ss 

{- INV. REP.:
   --Un empleado puede trabajar en mas de un sector id. 
   --El segundo map relacion el cuil de cada empleado con el empleado. 
   --El primer map relaciona id  de sectores con los empleados que trabajan en dicho sector. 
   --No hay sectores id repetidos. 
   --Cada empleado que trabaje en un sector id debera pertenecer al set de empleados de dicho sector id
   --Los sectores deben ser los mismos que los de las listas que tiene cada empleado.    -}
consEmpresa :: Empresa
-- Propósito: construye una empresa vacía.
-- Costo: O(1)
consEmpresa = ConsE emptyM emptyM
-- 
buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- Propósito: devuelve el empleado con dicho CUIL.
-- Precondición: el CUIL es de un empleado de la empresa.
-- Costo: O(log E)
buscarPorCUIL c (ConsE _ es) = lookupM c es 
-- 
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(log S + E)
empleadosDelSector sector (ConsE ss _) = case lookupM sector ss of 
                                       Just v -> setToList v 
                                       Nothing -> error "El sector no pertenece a la empresa dada"
-- 
todosLosCUIL :: Empresa -> [CUIL]
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosCUIL (ConsE ss es) = keys es 
-- 
todosLosSectores :: Empresa -> [SectorId]
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
todosLosSectores  (ConsE ss es) = keys ss 
-- 
agregarSector :: SectorId -> Empresa -> Empresa
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S)
agregarASector id (E ss es) = E (assocM id emptyS ss) es
-- 
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el
-- CUIL dado.
-- Costo: O (S log E) ?
agregarEmpleado is cuil (E ss es) = let emp = incorporarASectores (consEmpleado ss) in 
                            in (E  (agregarEmpleadoASectores emp is ss) (incorporarAEmpleados emp es))

incorporarAEmpleados :: Empleado -> Map CUIL Empleado -> Map CUIL Empleado 
incorporarAEmpleados emp mp = assocM (cuil emp) emp mp -- O(log E)?

agregarEmpleadoASectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarEmpleadoASectores    _ [] mp     = mp
agregarEmpleadoASectores    e (x:xs) mp = case lookupM x mp of 
                                           Just v -> (assocM x (addS e v)) agregarEmpleadoASectores e xs mp --O(log S) ?
                                           Nothing -> (assocM (addS emptyS)) agregarEmpleadoASectores e xs mp 

incorporarASectores :: Empleado -> [SectorId] -> Empleado
-- O (S log S) 
incorporarASectores   e [] = e 
incorporarASectores   e (i:is) =  incorporarSector i (incorporarASectores e is)

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: calcular
agregarASector    id cuil (ConsE ss es) = case lookupM cuil es of
                                           Just v -> let empleado = incorporarSector id v in 
                                                       in ConsE (agregarASectorM id empleado ss) (incorporarEmpleado empleado mp)
                                           Nothing -> ConsE ss es

incorporarEmpleado:: Empleado -> Map CUIL Empleado -> Map CUIL Empleado
incorporarEmpleado    emp     mp = assocM (cuil emp) emp mp --O(log E) ?

agregarASectorM :: SectorId -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
agregarASectorM    id e mp =   assocM id (addS e fromJust((lookupM id mp))) mp --O(log S) o O(S) ??

borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: 
borrarEmpleado cuil (ConsE ss es) = case lookupM cuil es of 
                                      Just v -> ConsE (borrarEmpleadoDeSectores (keys ss) v ss) (borrarEmpleadoM cuil es)
                                      Nothing -> ConsE ss es 

borrarEmpleadoM:: CUIL -> Map CUIL Empleado --O log E 
borrarEmpleadoM cuil mp = deleteM cuil mp 

borrarEmpleadoDeSectores:: [SectorID] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (SetEmpleado)
borrarEmpleadoDeSectores [] _ mp =  mp
borrarEmpleadoDeSectores (x:xs) emp mp = case lookupM x mp of 
                                          Just v -> (assocM x (borrarEmpleadoS emp v) ) borrarEmpleadoDeSectores xs emp mp
                                          Nothing -> borrarEmpleadoDeSectores xs emp mp 

borrarEmpleadoS:: Empleado -> Set Empleado -> Set Empleado 
borrarEmpleadoS emp set = removeS emp set --O(E)

fromJust :: Maybe b -> a --O(1)
fromJust Just v = v 

module Mago(Hechizo, Nombre, Mago, crearM, nombre, aprender, hechizos) where

import Set

type Hechizo = String
type Nombre  = String

data Mago = M Nombre (Set Hechizo)

crearM     :: Nombre -> Mago            -- O(1)
nombre     :: Mago -> Nombre            -- O(1)
aprender   :: Hechizo -> Mago -> Mago   -- O(log H)
hechizos   :: Mago -> Set Hechizo       -- O(1)
--(==), (<=) :: Mago -> Mago -> Bool      -- O(1)
-- Iguales, mismo nombre; menor, más hechizos

crearM   n          = M n emptyS
nombre     (M n _)  = n
aprender h (M n sh) = M n (addS h sh)
hechizos   (M _ sh) = sh

instance Eq Mago where
  (M n1 _) == (M n2 _) = n1 == n2

instance Ord Mago where
  (M _ sh1) <= (M _ sh2) = sizeS sh1 >= sizeS sh2
module EscuelaDeMagia(
    EscuelaDeMagia, fundarEscuela, estaVacia
                  , magos, registrar, hechizosDe
                  , leFaltaAprender, egresarUno, enseniar     
) where

import Mago
import Map
import Set
import PriorityQueue

data EscuelaDeMagia = EDM (Set Hechizo)         -- Todos los enseñados
                          (Map Nombre Mago)     -- Magos por nombre
                          (PriorityQueue Mago)  -- Magos por poder
  {- INV.REP.:
      * si un mago está en el Map, también debe estar en la PQ, y viceversa
         (y ser exactamente el mismo mago... -- mismos hechizos)
      * si cualquier mago tiene un hechizo, tiene que estar en el Set
      * en la PQ no hay dos magos con el mismo nombre
  -}

fundarEscuela   :: EscuelaDeMagia                              -- O(1)
estaVacia       :: EscuelaDeMagia -> Bool                      -- O(1)
magos           :: EscuelaDeMagia -> [Nombre]                  -- O(M)
registrar       :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia  -- O(log M)
hechizosDe      :: Nombre -> EscuelaDeMagia -> Set Hechizo     -- O(log M)   
leFaltaAprender :: Nombre -> EscuelaDeMagia -> Int             -- O(log M)
egresarUno      :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)    -- O(log M)
enseniar        :: Hechizo -> Nombre                           -- O(M log M
                    -> EscuelaDeMagia -> EscuelaDeMagia        --   + log H)


fundarEscuela = EDM emptyS emptyM emptyPQ

estaVacia (EDM _ _ p) = isEmptyPQ p
   -- ¿Vale usar (sizeS s == 0)?
   --   No. ¿Y si hubo magos y egresaron todos? ¡Va haber hechizos en una escuela vacía!
   -- ¿Vale usar (domM m == [])?
   --   No, porque quedaría costo O(M)
   -- ¿Necesito preguntar por el map?
   --   No, porque el invariante me garantiza que si una está vacía, la otra también.

magos (EDM _ mm _) = domM mm  -- keys m (con el nombre que puso Fede...)
   -- ¿Vale recorrer la PQ sacando los nombres de cada mago?
   --   No, porque quedaría costo O(M log M)

registrar n (EDM s mm p) = 
    case (lookupM n mm) of
      Nothing -> let m = crearM n
                  in EDM s (assocM n m mm) (insertPQ m p)
      Just _  -> EDM s mm p
    -- Debo buscarlo, porque si no violaría invariantes (y contrato)
    -- Debo agregarlo en ambos, para cumplir el inviarante
    -- No hace falta modificar el set, porque el nuevo mago no tiene hechizos...

hechizosDe n (EDM s mm p) = buscarHechizos n mm

buscarHechizos :: Nombre -> Map Nombre Mago -> Set Hechizo   -- O(log M)
buscarHechizos n mm = case (lookupM n mm) of
                        Nothing -> error "No estudia acá"
                        Just m  -> hechizos m
                      --ALTERNATIVA:
                      --let m = fromJust (lookupM n mm)                              
                      -- in hechizos m

leFaltaAprender n e@(EDM s mm _) = sizeS s - sizeS (hechizosDe n e)
    -- Vale usar funciones anteriores
    -- ALTERNATIVA: (para evitar el doble pattern matching) funciones auxiliares
    --   sizeS s - sizeS (buscarHechizos n mm)

egresarUno (EDM s mm p) = 
    let m = findMinPQ p
     in (m, EDM s (deleteM (nombre m) mm) (deleteMinPQ p)) 
     -- Se borra de los dos lados para mantener el invariante

enseniar h n (EDM s mm p) = 
    case (lookupM n mm) of
      Nothing -> error "No estudia acá"
      Just m  -> let newM = aprender h m
                  in EDM (addS h s) (assocM n newM mm)
                         (modificarPQ newM p)
    -- ¿Por qué es seguro usar modificarPQ, y no da error?
    --   Por el invariante, si m está en mm, también está en p                         

modificarPQ :: Mago -> PriorityQueue Mago 
                    -> PriorityQueue Mago        -- O(M log M)
-- PRECOND: tiene que haber un mago con el mismo nombre que el mago dado en la cola                    
modificarPQ m p = let mmp = findMinPQ p
                   in if (m == mmp)
                       then insertPQ m (deleteMinPQ p)
                       else insertPQ mmp (modificarPQ m (deleteMinPQ p))
hechizosDeEn :: [Nombre] -> EscuelaDeMagia -> Set Hechizo  -- O(M * (log M + H log H))
hechizosDeEn []     escuela = emptyS
hechizosDeEn (n:ns) escuela = unionS (hechizosDe n escuela)
                                     (hechizosDeEn ns escuela)
  -- hechizosDe es O(log M)
  -- unionS es O(H log H), porque el tamaño máximo de un conjunto de hechizos es H
  -- Entre ambos, da O(log M + H log H)
  --  Como sucede para cada nombre de la lista, que son M, entonces da
  --   O(M * (log M + H log H))                                    

hayUnExperto :: EscuelaDeMagia -> Bool
 -- Propósito: Indica si existe un mago que sabe todos los hechizos
 --            enseñados por la escuela.
 -- Eficiencia: O(log M)
hayUnExperto escuela = let (m, newEscuela) = egresarUno escuela
                        in leFaltaAprender (nombre m) escuela == 0
  -- El costo es log M, porque egresarUno y leFaltaAprender ambas son de costo log M

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
  -- Propósito: Devuelve un par con la lista de magos que saben todos
  --            los hechizos dados por la escuela y la escuela sin ellos.
  -- Eficiencia: O(M log M)
egresarExpertos escuela = if not (hayUnExperto escuela)
                           then ([], escuela)
                           else let (m , escuelaSinM)  = egresarUno escuela
                                    (ms, escuelaSinMs) = egresarExpertos escuelaSinM
                                 in (m : ms, escuelaSinMs)
   -- Las operaciones hayUnExperto, egresarUno son O(log M)
   -- Por cada mago (experto, que podrían ser todos...), usa esas dos funciones.
   -- Entonces, O(M log M)


deleteVH:: Ord a => n -> a -> Heap a -> Heap a
deleteVh   a 0 hp = hp
deleteVH   a n hp = if finMinH hp == a
                     then insertH a (deleteVH a (n-1)(deleteMin hp))
                     else deleteVH a (n-1) (deleteMin hp)

replaceAt :: Ord a => Int -> a -> Heap a -> Heap a
replaceAt 0 newVal hp = insertH newVal (deleteMin hp)
replaceAt n newVal hp 
  | isEmptyH hp = emptyH  -- En caso de que el Heap esté vacío
  | otherwise = insertH (findMin hp) (replaceAt (n - 1) newVal (deleteMin hp))