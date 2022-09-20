--Ejercicio 1 (A)

sucesor :: Int -> Int
sucesor n = n+1

--B)

sumar :: Int -> Int -> Int
sumar n m = n+m

--C)

divisionYResto :: Int -> Int -> (Int, Int)

divisionYResto x y = (div x y, mod x y)

--D)

maxDelPar (x, y) = if(x > y)
                   then x 
                   else y

                              {-Tipos Enumerativos-}

--ejercicio 1 A)

data Dir = Norte | Este | Sur | Oeste
     deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte
opuesto Oeste = Este 

--B)

iguales :: Dir -> Dir -> Bool
iguales Este Este = True
iguales Norte Norte = True
iguales Oeste Oeste = True
iguales Sur Sur = True
iguales _ _ = False

--C)
siguiente :: Dir -> Dir
--PRECONDICIÓN= La Dir no puede ser Oeste.
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste

--Ejercicio 2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
     deriving Show

-- A)

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia  = (primerDia, ultimoDia)

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

--B)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

--C)

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = numeroDia dia1 > numeroDia dia2

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes = 1
numeroDia Martes = 2
numeroDia Miercoles = 3
numeroDia Jueves = 4
numeroDia Viernes = 5
numeroDia Sabado = 6
numeroDia Domingo = 7

--D)
-- PRECONDICIÓN= Tiene que ser un dia de la semana válido. 
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

--Ejercicio 3)

--A)

negar :: Bool -> Bool
negar False = True
negar True = False

--B)

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _        = True

--C)

yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien True  a = a

--D)

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ a = a

                              {-REGISTROS-}

--Ejercicio 1
-- A)
data Persona = P String Int 
              -- Nombre Edad
     deriving Show

axel = P "Axel" 24

nombre ::Persona -> String
nombre (P n _) = n

--B)

edad :: Persona -> Int
edad (P _ e) = e

--C)

crecer :: Persona -> Persona
crecer (P n e) = P(n)
                   (e+1)

--D)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre  nom (P s  c) = P nom c

--E)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra v s = edad v > edad s

--F)

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor c m = if (esMayorQueLaOtra c m)
                   then c 
                   else m

--Ejercicio 2

data Pokemon = PK TipoDePokemon Int
                              --Numero (porcentaje energia)
     deriving Show

data TipoDePokemon = Agua | Fuego | Planta
     deriving Show

data Entrenador= E String Pokemon Pokemon
                -- Nombre  Pokemon Pokemon
     deriving Show

--A)

pk1 = PK Agua 50
pk2 = PK Planta 70
pk3 = PK Planta 32
pk4 = PK Agua 21
ent1 = E "Fulano" pk1 pk2
ent2 = E "Mengano" pk3 pk4

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (PK n _) = n

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = puedeSuperarA(tipoPokemon p1) (tipoPokemon p2)

puedeSuperarA :: TipoDePokemon ->TipoDePokemon -> Bool
puedeSuperarA Agua Fuego   = True
puedeSuperarA Fuego Planta = True
puedeSuperarA Planta Agua  = True
puedeSuperarA _ _          = False 


--B)

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipo (E _ p1 p2) =  unoSi (esDelMismoTipo tipo(tipoPokemon p1)) + 
                                              unoSi (esDelMismoTipo tipo(tipoPokemon p2))

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua     = True
esDelMismoTipo Fuego Fuego   = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo _ _           = False

--C)

juntarPokemon :: (Entrenador,Entrenador) -> [Pokemon]
juntarPokemon(ent1,ent2) =pokemones ent1 ++ pokemones ent2

pokemones :: Entrenador -> [Pokemon]
pokemones (E _ p1 p2) = p1:p2:[]

               {-Funciones Polimórficas-}

--Ejercicio 1

--A)

loMismo :: a -> a
loMismo x = x

--B)

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

                    {-Pattern matching sobre listas-}

--Ejercicio 2

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

--Ejercicio 3

elPrimero :: [a] -> a
elPrimero (x:xs) = x

--Ejercicio 4

sinElPrimero :: [a] -> [a]
sinElPrimero (x:xs) = xs

--Ejercicio 5

splitHead :: [a] -> (a, [a])
splitHead x = (elPrimero x, sinElPrimero x)