data Color = Azul | Rojo
   deriving Show
data Celda = Bolita Color Celda | CeldaVacia
   deriving Show

                       {-Tipos recursivos simples-}

--1.1)Celdas con bolitas

--I)

celda0 = CeldaVacia
celda1 = Bolita Azul CeldaVacia
celda2 = Bolita Rojo (Bolita Azul CeldaVacia)
celda3 = Bolita Azul (Bolita Rojo  CeldaVacia)

nroBolitas :: Color -> Celda -> Int
nroBolitas color CeldaVacia = 0
nroBolitas color (Bolita c cl) = unoSiEsColorEn color c + nroBolitas color cl

unoSiEsColorEn :: Color -> Color -> Int
unoSiEsColorEn c1 c2 = 1
unoSiEsColorEn _  _  = 0

--II)

poner :: Color -> Celda -> Celda
poner color celda= Bolita color celda

--III)

sacar :: Color -> Celda -> Celda
sacar color CeldaVacia = CeldaVacia
sacar color (Bolita c cv) = if esDelMismoColor color c
                            then cv
                            else Bolita color (sacar c cv)

esDelMismoColor :: Color -> Color -> Bool
esDelMismoColor Azul Azul = True
esDelMismoColor Rojo Rojo = True
esDelMismoColor _  _ = False

--IV)

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 color celda = celda
ponerN n color celda = poner color (ponerN (n-1) color celda)

--1.2) Camino hacía el tesoro

data Objeto = Cacharro | Tesoro
   deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
   deriving Show

camino0 = Nada camino4
camino1 = Cofre [ob1,ob2,ob3,ob4] camino2
camino2 = Cofre [ob2,ob3] camino3
camino3 = Cofre [ob3,ob4] camino4
camino4 = Fin

ob1 = Cacharro
ob2 = Tesoro
ob3 = Tesoro
ob4 = Tesoro

--I)

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro (Cofre objs camino) = hayTesorosEn(objs) || hayTesoro camino

hayTesorosEn :: [Objeto] -> Bool
hayTesorosEn [] = False
hayTesorosEn (o:os) = esTesoro o || hayTesorosEn os

esTesoro :: Objeto ->Bool
esTesoro Tesoro = True
esTesoro Cacharro = False

--II)

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = 0
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre objs camino) = if hayTesoro camino
                                  then 1 + pasosHastaTesoro camino
                                  else  pasosHastaTesoro camino

cantPasoTesoro :: [Objeto] -> Int
cantPasoTesoro [] = 0
cantPasoTesoro (o:os) = unoSi(noEsTesoro o) + cantPasoTesoro os

noEsTesoro :: Objeto -> Bool
noEsTesoro Tesoro = False
noEsTesoro _ = True

unoSi ::Bool ->Int
unoSi True = 1
unoSi False = 0

--III)

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n Fin = False
hayTesoroEn 0 _ = False
hayTesoroEn n (Nada camino) = hayTesoroEnPasos n (todosLosObjetos camino)

todosLosObjetos :: Camino -> [Objeto]
todosLosObjetos Fin                    = []
todosLosObjetos (Nada camino)          = todosLosObjetos camino
todosLosObjetos (Cofre objetos camino) = objetos

hayTesoroEnPasos :: Int -> [Objeto] -> Bool
hayTesoroEnPasos n [] = False
hayTesoroEnPasos n (o:os) =  esTesoro o || hayTesoroEnPasos (n-1) os

--IV)

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n Fin = False
alMenosNTesoros n (Nada c) = alMenosNTesoros n c
alMenosNTesoros n (Cofre objetos c) = cantTesoro objetos > n ||
                              alMenosNTesoros (n - cantTesoro objetos) c

cantTesoro :: [Objeto] -> Int
cantTesoro [] = 0
cantTesoro (o:os) = unoSi(esTesoro o) + cantTesoro os

--V)

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n1 n2 camino = cantTesoroHasta (n1-n2) (seguirPasosHastaAhora n1 camino)

seguirPasosHastaAhora :: Int -> Camino -> Camino
seguirPasosHastaAhora 0 camino = camino
seguirPasosHastaAhora n Fin    = Fin
seguirPasosHastaAhora n (Nada camino) = seguirPasosHastaAhora(n-1) camino
seguirPasosHastaAhora n (Cofre objs camino) = seguirPasosHastaAhora(n-1) camino

cantTesoroHasta :: Int -> Camino -> Int
cantTesoroHasta 0 camino = 0
cantTesoroHasta n Fin    = 0
cantTesoroHasta n (Nada camino) = cantTesoroHasta (n-1) camino
cantTesoroHasta n (Cofre objs camino) = cantTesoro objs + cantTesoroHasta(n-1) camino
               {-Tipos arbóreos-}

--2.1 Árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

a1 :: Tree Int
a1 = NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
                (NodeT 4 EmptyT EmptyT)
                )
                (NodeT 5 (NodeT 6 EmptyT EmptyT)
                EmptyT)

--I)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT e izq der) = e + sumarT izq + sumarT der

--II)

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT e izq der) = 1 + sizeT izq + sizeT der

--III)

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT e izq der) =(NodeT (e*2) (mapDobleT izq)(mapDobleT der))

--IV)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a EmptyT = False
perteneceT a (NodeT x t1 t2) = a==x ||perteneceT a t1 || perteneceT a t2

--V)

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a EmptyT = 0
aparicionesT a (NodeT x t1 t2) = unoSi(a==x) + aparicionesT a t1 + aparicionesT a t2

--VI)

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x t1 t2) = leaves t1 ++ leaves t2

--VII)

heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

--VIII)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = (NodeT x (mirrorT t2) (mirrorT t1))

-- IX)

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x EmptyT EmptyT) = [x]
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2

--X)

levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT x _ _) =  x : []
levelN n (NodeT x t1 t2) = levelN(n-1) t1 ++ levelN(n-1) t2

--XI)

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x t1 t2) = [x] : juntarTodosLosNiveles (listPerLevel t1) (listPerLevel t2)

juntarTodosLosNiveles :: [[a]] -> [[a]] -> [[a]]
juntarTodosLosNiveles _ yss = yss
juntarTodosLosNiveles xss _ = xss
juntarTodosLosNiveles (xs:xss) (ys:yss) = (xs++ys) : juntarTodosLosNiveles xss yss

--XII)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = x : laRamaMasLarga (ramaMasLarga t1) (ramaMasLarga t2)

laRamaMasLarga :: [a] -> [a] -> [a]
laRamaMasLarga t1 t2 = if longitud t1 > longitud t2
                     then t1
                     else t2

longitud :: [a] -> Int
longitud [] = 0
longitud(x:xs) = 1 + longitud xs

--XIII)

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = [x] : losCaminosEn x (todosLosCaminos t1) ++
               losCaminosEn x (todosLosCaminos t2)
losCaminosEn :: a -> [[a]] -> [[a]]
losCaminosEn y [] = []
losCaminosEn y (xs:xss) = (y:xs) : losCaminosEn y xss

--2.2) Expresiones Artiméticas

data ExpA = Valor Int| Sum ExpA ExpA| Prod ExpA ExpA | Neg ExpA
   deriving Show

exp0  = Valor 10 
exp1  = Valor 2 

sum0  = Sum  (exp0) (exp1) 
prod0 = Prod (exp1) (exp0)
neg0  = Neg  sum0

--I)

eval :: ExpA -> Int
eval (Valor e) = e
eval (Sum e1 e2) = eval e1 + eval e2
eval (Prod e2 e1) = eval e2 * eval e1
eval (Neg e) = -(eval e)

--II)

simplificar :: ExpA -> ExpA
simplificar (Valor e) = Valor e
simplificar (Sum e1 e2)  = simplifLaSuma (simplificar e1) (simplificar e2)
simplificar (Prod e2 e1) = simplificarProd (simplificar e2) (simplificar e1)
simplificar (Neg e)      = simpNeg (simplificar e)

simplifLaSuma :: ExpA -> ExpA -> ExpA
simplifLaSuma e (Valor 0) = e
simplifLaSuma (Valor 0) e = e
simplifLaSuma e1 e2 = Sum e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd e (Valor 0) = Valor 0
simplificarProd (Valor 0) e = Valor 0
simplificarProd e (Valor 1) = e
simplificarProd (Valor 1) e = e
simplificarProd e1 e2 = Prod e1 e2

simpNeg :: ExpA -> ExpA 
simpNeg (Neg n) = n
simpNeg n = Neg n