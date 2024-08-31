--2) NUMEROS ENTEROS
--  1) 
--a) Dado un número devuelve su sucesor.
sucesor :: Int -> Int
sucesor n = n + 1

--b) Dados dos números devuelve su suma utilizando la operación +.
sumar :: Int -> Int -> Int
sumar n m = n + m

--c) Dado dos números, devuelve un par donde la primera componente es la división del
--primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
--para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
--provista por Haskell.
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

--  3) Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Dena
--     las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
--     ya denidas en Haskell):

--a) Dado un booleano, si es True devuelve False, y si es False devuelve True.
--   En Haskell ya está denida como not.

negar :: Bool -> Bool
negar   True  =  False
negar   False =  True

--b) Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
--devuelve True.
--Esta función NO debe realizar doble pattern matching.
--Nota: no viene implementada en Haskell.
implica :: Bool -> Bool -> Bool
implica    True    b    =  b
implica    _       _    = True

--c) Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
-- Esta función NO debe realizar doble pattern matching.
-- En Haskell ya está denida como \&\&.

yTambien :: Bool -> Bool -> Bool
yTambien    True    b    =  b
yTambien    _       _    =  False

--d) Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
--Esta función NO debe realizar doble pattern matching.
--En Haskell ya está denida como ||.

oBien :: Bool -> Bool -> Bool
oBien    False   b    =  b
oBien    _       _    = True

--4) REGISTROS
--  1) Denir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
--     siguientes funciones:

data Persona = P String Int 
              -- Nombre Edad   
     deriving Show
--Casos de ejemplo:
mariana = P "Mariana" 34
julieta = P "Julieta" 28

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

--Casos de ejemplo:
balbusaur = PK Planta 3 
suicune = PK Agua 6 
charmander = PK Fuego 4 
ditto = PK Fuego 5 
lautaro = En "Lautaro" balbusaur suicune
federico = En "Federico" charmander ditto


--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
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

--5) FUNCIONES POLIMORFICAS
--a) Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo :: a -> a
loMismo    x =  x

--b) Dado un elemento de algún tipo devuelve el número 7.
siempreSiete :: a -> Int
siempreSiete    x =  7

--c) Dadas una tupla, invierte sus componentes.
swap :: (a,b) -> (b,a)
swap    (x,y) =  (y,x)

--6) PATTERM MATCHING SOBRE LISTAS

--2. Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--   Denida en Haskell como null.
estaVacia   ::  [a] ->  Bool
estaVacia       []  =   True
estaVacia       _   =   False

--3. Dada una lista devuelve su primer elemento.
--   Denida en Haskell como head.
elPrimero :: [a] -> a
elPrimero  (x:_) =  x

--4. Dada una lista devuelve esa lista menos el primer elemento.
--   Denida en Haskell como tail.
sinElPrimero :: [a] -> [a]
sinElPrimero (_:xs) =  xs

--5. Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--   lista, y la segunda componente es esa lista pero sin el primero.
splitHead :: [a] -> (a, [a])
splitHead    x   =  (elPrimero x, sinElPrimero x)
