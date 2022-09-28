module SetV2
  (Set,emptyS,addS,belongs,sizeS,removeS,unionS,setToList,s1,s2)
where

data Set a = S [a] deriving Show

s1 :: Set Int
s1 = S [1,2,3,4]

s2 :: Set Int
s2 = S [1,2,3,12,41]

emptyS :: Set a
--Crea un conjunto vacío.
emptyS = S []

addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS n (S xs) = S (n:xs)

belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs n (S xs) = pertenece n xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece k [] = False
pertenece k (x:xs) = (x == k) || pertenece k xs

sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs) = length xs

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS x (S ys) = S(quitarElemento x ys)

quitarElemento :: Eq a => a -> [a] -> [a]
quitarElemento e [] = []
quitarElemento e (x:xs) = if e == x
                          then xs 
                          else x : quitarElemento e xs 

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs) set = unirLosSets xs set

unirLosSets :: Eq a => [a] -> Set a -> Set a
unirLosSets [] s     = s
unirLosSets (x:xs) s = addS x (unirLosSets xs s)

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs) = sinRepetidos1 xs

sinRepetidos1 :: Eq a => [a] -> [a]
sinRepetidos1 []     = []
sinRepetidos1 (x:xs) = let ys = sinRepetidos1 xs
                                     in if pertenece x ys
                                         then     ys
                                         else x : ys