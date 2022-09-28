module SetV1 (Set,emptyS,addS,belongs,sizeS,removeS,unionS,setToList,s1,s2,s3)
where

data Set a =ConsS [a] Int 
 deriving Show
{- INV. REP. = * INT >= 0
               * El INT tiene que ser la cantidad de [a]
-}
s1 :: Set Int 
s1 = ConsS [1,2,3,4,5] 5

s2 :: Set Int 
s2 = ConsS [7,8,9,1] 4

s3 :: Set Int 
s3 = ConsS [5,17,9,5] 4

emptyS :: Set a
--Crea un conjunto vacío.
emptyS = ConsS [] 0
addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (ConsS xs n ) = if pertenece x xs
                    then ConsS xs n
                    else ConsS(x:xs) n
belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (ConsS xs _) = pertenece x xs 
sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (ConsS xs n) = length xs
removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS x (ConsS xs n) = ConsS (quitarElemento x xs) n

quitarElemento :: Eq a => a -> [a] -> [a]
quitarElemento e [] = []
quitarElemento e (x:xs) = if e == x
                          then xs 
                          else x : quitarElemento e xs 

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (ConsS xs n) s = unirLosSets xs s

unirLosSets :: Eq a => [a] -> Set a -> Set a
unirLosSets [] s     = s
unirLosSets (x:xs) s = addS x (unirLosSets xs s)

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.

setToList (ConsS xs n) = sinRepetidos1 xs



pertenece :: Eq a => a -> [a] -> Bool
pertenece k [] = False
pertenece k (x:xs) = (x == k) || pertenece k xs

sinRepetidos1 :: Eq a => [a] -> [a]
sinRepetidos1 []     = []
sinRepetidos1 (x:xs) = let ys = sinRepetidos1 xs
                                     in if pertenece x ys
                                         then     ys
                                         else x : ys 

