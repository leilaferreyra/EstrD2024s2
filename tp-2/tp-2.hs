
-- RECURSION SOBRE LISTAS
unoSi :: Bool -> Int  
unoSi    True = 1 
unoSi    False= 0 
--1. Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int]  -> Int
sumatoria    [   ]  =  0
sumatoria    (x:xs) =  x + sumatoria xs 

--2.Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
--  de elementos que posee.
longitud :: [a]   -> Int
longitud    [ ]    = 0 
longitud    (x:xs) = 1 + longitud xs

--3. Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores :: [Int]  -> [Int] 
sucesores    (x:xs) =  x + 1 : sucesores xs 
sucesores _ = []

--4. Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion :: [Bool] -> Bool
conjuncion    (x:xs) =  x && conjuncion xs 
conjuncion    [    ] =  True 

--5. Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion :: [Bool] -> Bool
disyuncion    (x:xs) =  x || disyuncion xs
disyuncion    [    ] =  False 

--6. Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar :: [[a]]  -> [a]
aplanar    (x:xs) =  x ++ aplanar xs 
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

 --11. Dados una lista y un elemento, devuelve una lista con ese elemento agregado al nal de la lista.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal    [ ]    e =  [e]
agregarAlFinal    (x:xs) e =  x : agregarAlFinal xs e

--12. Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Denida en Haskell como (++).
agregar :: [a] -> [a] -> [a]
agregar    [ ]    ys  =  ys
agregar    (x:xs) ys  =  x : agregar xs ys

--13. Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Denida en Haskell como reverse.
invertir :: [a]    -> [a]
invertir    [ ]    =  [ ]
invertir    (x:xs) =  invertir xs ++ [x]

--14. Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--    máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--    las listas no necesariamente tienen la misma longitud.
zipMaximos :: [Int] -> [Int]  -> [Int]
zipMaximos     _       [   ]  =  [   ]
zipMaximos     [  ]    _      =  [   ]
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
factorial    n   =  n * factorial (n-1) --Preguntar si hacer caso de error en caso negativo

--2. Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--   n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva    0   =  [   ]
cuentaRegresiva    n   =  n : cuentaRegresiva (n-1)

--3. Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir    0      _ =  [ ]
repetir    n      e =  e : repetir (n-1) e

--4. Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
--   Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 
--5. Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--   recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a]    -> [a] 
sinLosPrimeros    0      xs     =  xs
sinLosPrimeros    _      []     =  [ ]
sinLosPrimeros    n      (_:xs) =  sinLosPrimeros (n-1) xs 
                           
--REGISTROS
--1. Denir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
--siguientes funciones:
data Persona = P String Int 
              -- Nombre Edad   
     deriving Show
--Casos de ejemplo:
mariana = P "Mariana" 34
julieta = P "Julieta" 28
maria   = P "Maria"   40
juan    = P "Juan"    50

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
     deriving (Eq, Show)

data Pokemon = ConsPokemon TipoDePokemon Int 
     deriving (Show)

data Entrenador = ConsEntrenador String [Pokemon] 
     deriving (Show)
--Casos de ejemplo: 
juano = ConsEntrenador "Juano" [balbusaur,suicune,charmander]
fede = ConsEntrenador "Fede" [charmander]
balbusaur = ConsPokemon Planta 3 
suicune = ConsPokemon Agua 6 
charmander = ConsPokemon Fuego 4 

--Devuelve la cantidad de Pokémon que posee el entrenador.
pokemonesDe :: Entrenador             -> [Pokemon]
pokemonesDe    (ConsEntrenador _ ps)  =  ps

cantPokemon :: Entrenador -> Int
cantPokemon    e          =  longitud (pokemonesDe e)

--Devuelve la cantidad de Pokémon de determinado tipo    que posee el entrenador.
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe    t                e          =  cantPokemonDeAux t (pokemonesDe e)

cantPokemonDeAux :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDeAux    _                [       ] =  0 
cantPokemonDeAux    t                (p:ps)    =  unoSi (t == tipo p) + cantPokemonDeAux t ps
                       
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
--a los Pokemon del segundo entrenador.
tipo :: Pokemon           -> TipoDePokemon
tipo    (ConsPokemon t _) =  t

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_    t e1 e2 = contarVencedores t (pokemonesDe e1) (pokemonesDe e2)

contarVencedores:: TipoDePokemon ->  [Pokemon] -> [Pokemon] -> Int
contarVencedores  _ [       ]    _         =  0
contarVencedores    t (p1:ps1)    ps2  = (unoSi ((esDeTipo p1 t) && (superaATodos p1 ps2))) + contarVencedores t ps1 ps2 

esDeTipo :: Pokemon -> TipoDePokemon -> Bool
esDeTipo    pk1        t             =  (tipo p1) == t

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo    Agua              Agua     = True
esMismoTipo    Planta           Planta = True
esMismoTipo

superaATodos:: Pokemon -> [Pokemon] -> Bool
superaATodos  _    [ ] = True 
superaATodos   pk1        (pk2:ps)     =  superaA pk1 pk2 && superaATodos pk1 ps 

mejorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
mejorTipo    Agua             Fuego         =  True
mejorTipo    Fuego            Planta        =  True
mejorTipo    Planta           Agua          =  True
mejorTipo    _                _             =  False

--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon    e          =  hayUnoDeCada (pokemonesDe e)

hayUnoDeCada :: [Pokemon] -> Bool
hayUnoDeCada    [       ] =  False 
hayUnoDeCada    ps        =  hayAgua ps && hayFuego ps && hayPlanta ps

hayAgua :: [Pokemon] -> Bool
hayAgua    [       ] =  False 
hayAgua    (p:ps)    = (tipo p) == Agua || hayAgua ps

hayFuego :: [Pokemon] -> Bool
hayFuego    [       ] = False 
hayFuego    (p:ps)    = (tipo p) == Fuego || hayFuego ps

hayPlanta :: [Pokemon] -> Bool
hayPlanta    [       ] = False 
hayPlanta    (p:ps)    = (tipo p) == Planta || hayPlanta ps

--3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
--de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
--una lista de personas con diferente rol. La denición es la siguiente:
data Seniority = Junior | SemiSenior | Senior 
     deriving Show
data Proyecto = ConsProyecto String 
     deriving (Eq, Show)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto   
     deriving Show 
data Empresa = ConsEmpresa [Rol] 
     deriving Show 

--Casos de ejemplo:
jumbo = (ConsEmpresa [juani, feli , fran ])
juani = Developer Senior proyecto1
feli  = Developer Senior proyecto2 
fran  = Developer Senior proyecto1 
proyecto1 = (ConsProyecto "AB")
proyecto2 = (ConsProyecto "ZB")

--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos :: Empresa ->             [Proyecto]
proyectos    e = proyectos' rolesDe e 

proyectos' :: [Rol] -> [Proyecto]
proyectos' [] _ = []
proyectos' (r:rs) = agregarSiNoExiste (proyecto r) (proyectos'rs) 
agregarSiNoExiste :: Proyecto -> [Proyecto] -> [Proyecto]
agregarSiNoExiste    p           [        ] =  [p]
agregarSiNoExiste    p           (p2:ps)    = if esMismoProyecto p p2 
                                              then p2:ps 
                                              else p2:agregarSiNoExiste p ps 

esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto    (ConsProyecto p1)       (ConsProyecto p2)       =  p1 == p2



proyecto :: Rol               -> Proyecto 
proyecto    (Developer _ p)   = p
promedio    (Management _ p)  = p
                     
--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
--además a los proyectos dados por parámetro.
losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior  e  ps = losDevSenior' (rolesDe e ) ps 

rolesDe :: Empresa -> [Rol]
rolesDe (ConsEmpresa rs) = rs

losDevSenior':: [Rol] -> [Proyecto] -> Int 
losDevSenior' []    _     =  0 
losDevSenior' (r:rs) ps = unoSi (esDevSenior r && perteneceAAlgunProyecto r ps) + losDevSenior rs ps

perteneceAAlgunProyecto :: Rol -> [Proyecto] -> Bool
perteneceAAlgunProyecto    _      [        ] = False 
perteneceAAlgunProyecto    r      ps         = pertenece (proyecto r) ps
          
esDevSenior :: Rol -> Bool 
esDevSenior (Developer Senior _ ) = True 
esDevSenior _                     = False 

--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn :: [Proyecto] -> Empresa   -> Int
cantQueTrabajanEn    [        ]    _         =  0
cantQueTrabajanEn    _    (ConsEmpresa [])   =  0
cantQueTrabajanEn    ps (ConsEmpresa (r:rs)) =  unoSi (perteneceAAlgunProyecto r ps) + cantQueTrabajanEn ps (ConsEmpresa rs) 

--Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
--cantidad de personas involucradas.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto    rs      =  asignadosPorProyecto' (proyectos rs) rs 

asignadosPorProyecto' :: [Proyecto] -> Empresa   -> [(Proyecto,Int)]
asignadosPorProyecto'    [        ]    _         =  [              ]
asignadosPorProyecto'    _    (ConsEmpresa [])   =  [              ]
asignadosPorProyecto'    (p:ps) (ConsEmpresa rs) = (p, cantDeRolEnProyecto p (ConsEmpresa rs)):asignadosPorProyecto' ps (ConsEmpresa rs)

cantDeRolEnProyecto :: Proyecto -> Empresa              -> Int
cantDeRolEnProyecto    p           (ConsEmpresa [])     =  0
cantDeRolEnProyecto    p           (ConsEmpresa (r:rs)) = unoSi (p == (proyecto r)) + cantDeRolEnProyecto p (ConsEmpresa rs)
