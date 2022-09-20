                    {-Recursión sobre listas-}

--Ejercicio 1)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria(x:xs) = x + sumatoria xs

--Ejercicio 2

longitud :: [a] -> Int
longitud [] = 0
longitud(x:xs) = 1 + longitud xs

--Ejercicio 3

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = x+1: sucesores xs

--Ejercicio 4

conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (n:ns) = n && conjuncion ns

--Ejercicio 5

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (n:ns) = n || disyuncion ns

--Ejercicio 6

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

--Ejercicio 7

pertenece :: Eq a => a -> [a] -> Bool
pertenece k [] = False
pertenece k (x:xs) = (x ==k) || pertenece k xs

--Ejercicio 8

apariciones :: Eq a => a -> [a] -> Int 
apariciones k []     = 0
apariciones k (x:xs) = unoSi (k == x)+ apariciones k xs

unoSi ::Bool ->Int
unoSi True = 1
unoSi False = 0

--Ejercicio 9

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA k [] = []
losMenoresA k (n:ns) = if(n<k)
                       then n:losMenoresA k ns
                       else losMenoresA k ns

--Ejercicio 10

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA k [] = []
lasDeLongitudMayorA k (ns:nss) = if longitud ns > k
                                 then ns : lasDeLongitudMayorA k nss
                                 else lasDeLongitudMayorA k nss 

--Ejercicio 11

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] k = [k]
agregarAlFinal (x:xs) k = x : agregarAlFinal xs k

--Ejercicio 12

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar(x:xs) ys = x: agregar xs ys 

--Ejercicio 13

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs)  x

--Ejercicio 14

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos xs   [] = xs
zipMaximos []   ys = ys
zipMaximos (x:xs) (y:ys) = maxEntre x y : zipMaximos xs ys

maxEntre :: Int -> Int -> Int
maxEntre k l = if k > l
                  then k
                  else l

--Ejercicio 15

elMinimo :: Ord a => [a] -> a
elMinimo [] = error "la lista no puede ser vacía"
elMinimo (x: []) = x
elMinimo (n:ns) = minEntre n (elMinimo ns)

minEntre :: Ord a => a -> a -> a
minEntre k l = if k < l
               then k
               else l

                            {-RECURSIÓN SOBRE NÚMEROS-}

--Ejercicio 1

factorial :: Int -> Int
--PRECOND= No puede ser negativo.
factorial 0 = 1
factorial n = n * factorial(n-1)

--Ejercicio 2

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = [] 
cuentaRegresiva n = if n <1 
                    then []
                    else n : cuentaRegresiva (n-1)

--Ejercicio 3

repetir :: Int -> a -> [a]
repetir 0 x = []
repetir n x = x : repetir (n-1) x

--Ejercicio 4

losPrimeros :: Int -> [a] -> [a]
losPrimeros n [] = []
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--Ejercicio 5

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 (x:xs)  = (x:xs)
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros(n-1) xs

                                    {-REGISTROS-}

--Ejercicio 1

data Persona = P String Int
              -- Nombre Edad
    deriving Show

p1 = P "as" 40
p2 = P "asd" 10
p3 = P "adf" 20
p4 = P "afs" 30

pers :: [Persona]
pers = [p1,p2,p3,p4]

--I)

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = [] 
mayoresA n (x:xs) = singularSi x (edad x > n) ++ mayoresA n xs

singularSi :: a -> Bool -> [a]
singularSi x True = [x]
singularSi x False = []

edad :: Persona -> Int
edad (P _ e) = e 

--II)
promedioEdad :: [Persona] -> Int
promedioEdad [] = 0
promedioEdad ns = div (todasLasEdades ns) (longitud ns)

todasLasEdades :: [Persona] -> Int
todasLasEdades [] = 0
todasLasEdades (x:xs) = edad x + todasLasEdades xs

--III)

elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "la lista es vacía"
elMasViejo (p:[]) = p
elMasViejo (p:ps) = if (tieneMayorEdad p (elMasViejo ps))
                     then p
                     else elMasViejo ps

tieneMayorEdad :: Persona -> Persona-> Bool
tieneMayorEdad p1 p2 = edad p1 > edad p2
--Ejercicio 2

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int
    deriving Show
data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show

--I)
pok1 =[pk1,pk2,pk3]
pok2 =[pk4,pk5,pk6] 
pk1 = ConsPokemon Fuego 50
pk2 = ConsPokemon Fuego 40
pk3 = ConsPokemon Fuego 30

pk4 = ConsPokemon Planta 50
pk5 = ConsPokemon Planta 40
pk6 = ConsPokemon Fuego 30

ent1 = ConsEntrenador "mengano" pok1
ent2 = ConsEntrenador "fulano" pok2

cantPokemon :: Entrenador -> Int
cantPokemon ( ConsEntrenador _ pk) = longitud pk

--II)

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (ConsEntrenador _ pk) = length (cantidadDelMismoTipo tp pk)

cantidadDelMismoTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
cantidadDelMismoTipo tp [] = []
cantidadDelMismoTipo tp (p:ps) = if esDelMismoTipo tp (tipoPokemon p)
                                 then p : cantidadDelMismoTipo tp ps
                                 else cantidadDelMismoTipo tp ps

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (ConsPokemon tipo _) = tipo

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua     = True
esDelMismoTipo Fuego Fuego   = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo _ _           = False

--III)

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tp (ConsEntrenador s1 []) (ConsEntrenador s2 pk2)     = 0
losQueLeGanan tp (ConsEntrenador s1 pk1) (ConsEntrenador s2 pk2) = (sumaSiLeGanaATodos(head pk1) pk2) 
                          + losQueLeGanan tp (ConsEntrenador s1 (tail pk1))(ConsEntrenador s2 pk2)

sumaSiLeGanaATodos :: Pokemon -> [Pokemon] -> Int
sumaSiLeGanaATodos pk [] = 1
sumaSiLeGanaATodos pk (p:ps) = if superaA pk p
                               then sumaSiLeGanaATodos pk ps
                                else 0

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = puedeSuperarA(tipoPokemon p1) (tipoPokemon p2)

puedeSuperarA :: TipoDePokemon ->TipoDePokemon -> Bool
puedeSuperarA Agua Fuego   = True
puedeSuperarA Fuego Planta = True
puedeSuperarA Planta Agua  = True
puedeSuperarA _ _          = False 

--IV)

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ pk)= tieneTodoTipoDePokemon pk

tieneTodoTipoDePokemon :: [Pokemon] -> Bool
tieneTodoTipoDePokemon ps = tieneDeTipo Agua ps && tieneDeTipo Fuego ps && tieneDeTipo Planta ps

tieneDeTipo :: TipoDePokemon -> [Pokemon] -> Bool
tieneDeTipo tp [] = False
tieneDeTipo tp (p:ps) = esDelMismoTipo tp (tipoPokemon p) || tieneDeTipo tp ps

--Ejercicio 3

data Seniority = Junior | SemiSenior | Senior
    deriving Show
data Proyecto = ConsProyecto String
    deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show
data Empresa = ConsEmpresa [Rol] 
    deriving Show

emp1 = ConsEmpresa [rol1,rol2,rol3]

rol1 = Management Junior pr1 
rol2 = Developer Senior pr3
rol3 = Management Junior pr3

pr1 = ConsProyecto "Desarrollo web"
pr2 = ConsProyecto "Senior QA"
pr3 = ConsProyecto "Backend"
pr4 = ConsProyecto "Analista de Sistema"
pr5 = ConsProyecto "hacking"
pr6 = ConsProyecto "Frontend"

proyects = [pr1,pr2,pr3,pr4,pr5,pr6]

--I)

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa r) = sinProyectosARepetir (todosLosProyectos r)

todosLosProyectos :: [Rol] -> [Proyecto]
todosLosProyectos [] = []  
todosLosProyectos (r:rs) =  proyectoDelRol r : todosLosProyectos rs

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ proy) = proy
proyectoDelRol (Management _ proy) = proy

sinProyectosARepetir :: [Proyecto] -> [Proyecto]
sinProyectosARepetir [] = []
sinProyectosARepetir (p:ps) = if elProyectoPertenece p (sinProyectosARepetir ps )
                              then sinProyectosARepetir ps
                              else p : sinProyectosARepetir ps

elProyectoPertenece :: Proyecto -> [Proyecto] -> Bool
elProyectoPertenece proyecto []     = False 
elProyectoPertenece proyecto (p:ps) =  mismosProyectos proyecto p ||
                                                     elProyectoPertenece proyecto ps

mismosProyectos :: Proyecto -> Proyecto -> Bool
mismosProyectos  (ConsProyecto p1) (ConsProyecto p2) = p1 == p2 

--II)

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior _ [] = 0
losDevSenior (ConsEmpresa rs) ps = cantDevSenior rs ps

cantDevSenior :: [Rol] -> [Proyecto] -> Int
cantDevSenior [] _ = 0
cantDevSenior (r:rs) ps = unoSi (esDeveloperYTrabajoEnElProyecto r ps) + cantDevSenior rs ps

esDeveloperYTrabajoEnElProyecto :: Rol -> [Proyecto] -> Bool
esDeveloperYTrabajoEnElProyecto rol ps = esDev rol && perteneceAlProyecto rol ps

esDev :: Rol -> Bool
esDev (Developer _ _) = True
esDev (Management _ _) = False

perteneceAlProyecto :: Rol -> [Proyecto] -> Bool
perteneceAlProyecto rol [] = False
perteneceAlProyecto rol (p:ps) = trabajoProyecto rol p || perteneceAlProyecto rol ps

trabajoProyecto :: Rol -> Proyecto -> Bool
trabajoProyecto r p = mismosProyectos (proyectoDelRol r) p

--III)

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] _ = 0
cantQueTrabajanEn ps (ConsEmpresa roles) = totalQueTrabajanEn ps roles

totalQueTrabajanEn :: [Proyecto] -> [Rol] -> Int
totalQueTrabajanEn _ [] = 0
totalQueTrabajanEn ps (r:rs) = unoSi (perteneceAlProyecto r ps) + totalQueTrabajanEn ps rs

--IV)

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = proyectosAsignados rs

proyectosAsignados :: [Rol] -> [(Proyecto, Int)]
proyectosAsignados [] = []
proyectosAsignados (r:rs) = asignar (proyectoDelRol r) (proyectosAsignados rs)

asignar :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
asignar pr [] = [(pr, 1)]
asignar pr ((p,j):pjs) = if mismosProyectos p pr
                        then (p, j+1) : pjs
                        else (p, j) : asignar pr pjs
