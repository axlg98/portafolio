       {-1) PIZZAS-}

data Pizza = Prepizza | Capa Ingrediente Pizza
   deriving Show
data Ingrediente = Salsa| Queso| Jamon| Aceitunas Int
   deriving Show

pizza0 = Prepizza
pizza1 = Capa Salsa (Capa Queso Prepizza)
pizza2 = Capa Salsa(Capa Jamon (Capa (Aceitunas 8) Prepizza))

ingredientes= [Salsa, Queso,Jamon]
pizzas = [pizza0,pizza1, pizza2]
--Ejercicio 1)

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa ing pre) = unoSi(hayIng ing) + cantidadDeCapas pre

hayIng :: Ingrediente -> Bool
hayIng Salsa = True
hayIng Queso = True
hayIng Jamon = True
hayIng (Aceitunas n) = True
hayIng _ = False

unoSi ::Bool ->Int
unoSi True = 1
unoSi False = 0

--Ejercicio 2)

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (i:is) = Capa i (armarPizza is)

--Ejercicio 3)

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing pre) = if esJamon ing
                             then sacarJamon pre 
                             else Capa ing (sacarJamon pre)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

--Ejercicio 4)

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza = True
tieneSoloSalsaYQueso (Capa ing pre) = tieneSoloSalsaOQueso ing && tieneSoloSalsaYQueso pre

tieneSoloSalsaOQueso :: Ingrediente -> Bool
tieneSoloSalsaOQueso ing = esQueso ing || esSalsa ing

esQueso :: Ingrediente -> Bool
esQueso Queso = True
esQueso _     = False

esSalsa :: Ingrediente -> Bool
esSalsa Salsa = True
esSalsa _     = False

--Ejercicio 5

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza      = Prepizza
duplicarAceitunas(Capa ing pre) = if esAceitunas ing
                                  then Capa ing (Capa ing(duplicarAceitunas pre))
                                  else Capa ing (duplicarAceitunas pre)

esAceitunas :: Ingrediente -> Bool
esAceitunas (Aceitunas n) = True
esAceitunas _ = False

--Ejercicio 6

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p,p) :cantCapasPorPizza ps

             {-2. Mapa de tesoros (con bifurcaciones)-}

data Dir = Izq | Der
   deriving Show
data Objeto = Tesoro | Chatarra
   deriving Show
data Cofre = Cofre [Objeto]
   deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
   deriving Show

cofre1 = Cofre [Chatarra, Chatarra, Chatarra] 

cofre2 = Cofre [Chatarra, Chatarra, Tesoro]

cofre3 = Cofre [Chatarra, Chatarra, Chatarra, Tesoro]

cofre5 = Cofre []

mapa1 = Fin cofre1
mapa2 = Fin cofre2

mapa3 = Bifurcacion cofre3 mapa1 mapa2

mapa4 = Bifurcacion cofre5 mapa1 mapa3


dir = [Izq,Der,Der,Izq]

--Ejercicio 1

hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre) = esTesoroEn cofre
hayTesoro (Bifurcacion cofre m1 m2) = esTesoroEn cofre || hayTesoro m1 || hayTesoro m2

esTesoroEn :: Cofre -> Bool
esTesoroEn (Cofre objs) = hayTesorosEn objs

hayTesorosEn :: [Objeto] -> Bool
hayTesorosEn [] = False
hayTesorosEn (o:os) = esTesoro o || hayTesorosEn os

esTesoro :: Objeto ->Bool
esTesoro Tesoro = True
esTesoro _ = False

--Ejercicio 2

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] m = hayTesoroEnEsteCamino m
hayTesoroEn (d:ds) (Fin cofre) = error "se terminó el camino"
hayTesoroEn (d:ds) (Bifurcacion cofre m1 m2) = case d of
                                               Izq -> hayTesoroEn ds m1
                                               Der -> hayTesoroEn ds m2

hayTesoroEnEsteCamino :: Mapa -> Bool
hayTesoroEnEsteCamino (Fin cofre)      = hayTesoroEnElCofre cofre
hayTesoroEnEsteCamino (Bifurcacion cofre m1 m2) = hayTesoroEnElCofre cofre

hayTesoroEnElCofre :: Cofre -> Bool
hayTesoroEnElCofre (Cofre objs) = hayTesoroEntreLosObjetos objs 

hayTesoroEntreLosObjetos :: [Objeto] -> Bool
hayTesoroEntreLosObjetos [] = False
hayTesoroEntreLosObjetos (o:os) = esTesoro o || hayTesoroEntreLosObjetos os

--Ejercicio 3

caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre) = []
caminoAlTesoro mapa = if esTesoroEn (esCofre mapa)
                      then []
                      else caminoHacíaElTesoroPorElMapa mapa

caminoHacíaElTesoroPorElMapa :: Mapa -> [Dir]
caminoHacíaElTesoroPorElMapa (Fin cofre) = []
caminoHacíaElTesoroPorElMapa (Bifurcacion cofre mi md) = if hayTesoro mi
                                                         then Izq : caminoHacíaElTesoroPorElMapa mi
                                                         else Der : caminoHacíaElTesoroPorElMapa md

esCofre :: Mapa -> Cofre
esCofre (Fin cofre)             = cofre
esCofre (Bifurcacion cofre _ _) = cofre

--Ejercicio 4

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin cofre) = []
caminoDeLaRamaMasLarga (Bifurcacion cofre mi md) = 
                         if length (caminoDeLaRamaMasLarga mi) > length(caminoDeLaRamaMasLarga md)
                         then Izq : caminoDeLaRamaMasLarga mi
                         else Der : caminoDeLaRamaMasLarga md

--Ejercicio 5

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin cofre) = tesorosEn cofre : []
tesorosPorNivel (Bifurcacion cofre mi md) = tesorosEn cofre :
                             juntarNiveles (tesorosPorNivel mi) (tesorosPorNivel md) 


tesorosEn :: Cofre ->[Objeto]
tesorosEn (Cofre []) = []
tesorosEn (Cofre objs) = todosLosTesorosEnObjetos objs

todosLosTesorosEnObjetos :: [Objeto] -> [Objeto]
todosLosTesorosEnObjetos [] =[]
todosLosTesorosEnObjetos (o:os) = singularSi o (esTesoro o) ++ todosLosTesorosEnObjetos os

singularSi :: a -> Bool -> [a]
singularSi x True = [x]
singularSi x False = []

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles xss [] = xss 
juntarNiveles [] yss = yss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

--Ejercicio 6

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin cofre) = [[]]
todosLosCaminos (Bifurcacion cofre m1 m2) = [] : agregarDir Izq (todosLosCaminos m1) ++ 
                                                            agregarDir Der (todosLosCaminos m2)

agregarDir :: Dir -> [[Dir]] -> [[Dir]]
agregarDir d []       = [[]]
agregarDir d (ds:dss) = (d:ds) : agregarDir d dss

          {-3. Nave Espacial-}

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

nave0 = N treesector0
treesector0 = NodeT sector0 (NodeT sector1 EmptyT EmptyT) EmptyT
sector0 = S "123" comp0 trip0
comp0 = [LanzaTorpedos, (Motor 50), (Almacen [Comida, Oxigeno, Combustible]), LanzaTorpedos]
trip0= ["a", "b", "c", "d"]

sector1 = S "345" comp1 trip1
comp1 = [(Motor 70), LanzaTorpedos]
trip1 = ["w","e","q"]
comp2 = [LanzaTorpedos, (Motor 150)]

sect= ["345", "123"]
--Ejercicio 1

sectores :: Nave -> [SectorId]
sectores (N treeSector) = sectoresDe treeSector

sectoresDe :: Tree Sector -> [SectorId]
sectoresDe EmptyT  = []
sectoresDe (NodeT x t1 t2) = idSector x : (sectoresDe t1 ++ sectoresDe t2)

idSector :: Sector -> SectorId
idSector (S id _ _) = id

--Ejercicio 2

poderDePropulsion :: Nave -> Int
poderDePropulsion (N treeSector) = cantDePropulsiónEn treeSector

cantDePropulsiónEn :: Tree Sector -> Int
cantDePropulsiónEn EmptyT = 0
cantDePropulsiónEn (NodeT sector t1 t2) = cantidadPropulsión sector + 
                                         cantDePropulsiónEn t1 + cantDePropulsiónEn t2

cantidadPropulsión :: Sector -> Int 
cantidadPropulsión (S _ componente _ ) = cantPoderDelMotorEn componente

cantPoderDelMotorEn :: [Componente] -> Int
cantPoderDelMotorEn [] = 0
cantPoderDelMotorEn (c:cs) = poderDelMotor c + cantPoderDelMotorEn cs

poderDelMotor :: Componente -> Int
poderDelMotor c = if esMotor c 
                  then cantPoderMotor c
                  else 0

esMotor :: Componente -> Bool
esMotor (Motor n) = True
esMotor _         = False

cantPoderMotor :: Componente -> Int
cantPoderMotor (Motor n) = n

--Ejercicio 3 

barriles :: Nave -> [Barril]
barriles (N treeSector) = cantDeBarrilesEnElSector treeSector

cantDeBarrilesEnElSector :: Tree Sector -> [Barril]
cantDeBarrilesEnElSector EmptyT               = []
cantDeBarrilesEnElSector (NodeT sector t1 t2) = todosLosBarrilesDelSector sector ++
                              cantDeBarrilesEnElSector t1 ++ cantDeBarrilesEnElSector t2

todosLosBarrilesDelSector :: Sector -> [Barril]
todosLosBarrilesDelSector (S _ componente _) = todosLosBarrilesEnElComponente componente

todosLosBarrilesEnElComponente :: [Componente] -> [Barril]
todosLosBarrilesEnElComponente []     = []
todosLosBarrilesEnElComponente (c:cs) = if esBarrilEnElAlmacen c
                                        then agregarBarril c ++ todosLosBarrilesEnElComponente cs
                                        else todosLosBarrilesEnElComponente cs

esBarrilEnElAlmacen :: Componente -> Bool
esBarrilEnElAlmacen (Almacen bs) = True
esBarrilEnElAlmacen _            = False 

agregarBarril :: Componente -> [Barril]
agregarBarril (Almacen bs) = bs
agregarBarril _            = [] 

--Ejercicio 4

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs idSector (N treeSector) = 
                               N(agregarLosComponentesEnElSectorDado cs idSector treeSector)

agregarLosComponentesEnElSectorDado :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarLosComponentesEnElSectorDado cs idSec EmptyT = EmptyT
agregarLosComponentesEnElSectorDado cs idSec (NodeT sector si sd) =
                                          if idSec == idSector sector
                                          then NodeT (agregarComponente sector cs) si sd
                                          else NodeT sector (agregarLosComponentesEnElSectorDado cs idSec si)
                                                (agregarLosComponentesEnElSectorDado cs idSec sd)

agregarComponente :: Sector -> [Componente] -> Sector
agregarComponente (S sId comps trips) compsNuevos = (S sId (comps++compsNuevos)trips)

--Ejercicio 5

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA nTrip listIdS (N treeSector) = N (agregarElTripulanteEnLosSectores nTrip listIdS treeSector)

agregarElTripulanteEnLosSectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
agregarElTripulanteEnLosSectores trip ids EmptyT = EmptyT
agregarElTripulanteEnLosSectores trip [] treeSector = treeSector
agregarElTripulanteEnLosSectores trip ids (NodeT sector ti td) = 
           if perteneceAlSector (idSector sector) ids
           then NodeT (agregarAlTripulante trip sector) 
                 (agregarElTripulanteEnLosSectores trip ids ti) (agregarElTripulanteEnLosSectores trip ids td) 
           else NodeT sector (agregarElTripulanteEnLosSectores trip ids ti) (agregarElTripulanteEnLosSectores trip ids td)

perteneceAlSector :: SectorId -> [SectorId] -> Bool
perteneceAlSector idDelSector  [] = False 
perteneceAlSector idDelSector (id:ids) =idDelSector == id || perteneceAlSector idDelSector ids

agregarAlTripulante :: Tripulante -> Sector -> Sector
agregarAlTripulante nTrip (S id comps tripulacion) = S id comps (nTrip : tripulacion)

--Ejercicio 6

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados trip (N treeSector) = 
   asdas