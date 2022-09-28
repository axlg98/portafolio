import SetV1

--import QueueV2

import StackV1

import QueueDosListas

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
t1 = EmptyT
t2= NodeT s2( NodeT s1 EmptyT EmptyT) EmptyT
------------------{-EJERCICIO 1-}-----------------------
{-
--0(1)
head' :: [a] -> a
head' (x:xs) = x

--0(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

--0(n)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--0(n)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--0(n*m)
--Donde n es la longitud de la lista y
-- m es la media de los elementos de la misma.
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

--0(n^2)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

--0(n^2)
--sinRepetidos :: Eq a => [a] -> [a]
--sinRepetidos [] = []
--sinRepetidos (x:xs) = if pertenece x xs
--                      then sinRepetidos xs
--                      else x : sinRepetidos xs

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) = if n == x 
                 then xs 
                 else x : sacar n xs

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs = let m = minimo xs
             in m : ordenar (sacar m xs)
-}
----------------- {-EJERCICIO 2-}-----------------------
-- ====================
-- =         SET      =
-- ====================

--Ejercicio 2


--I

--0(n)
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []  s    = []
losQuePertenecen (x:xs) s = if belongs x s
                            then x : losQuePertenecen xs s
                            else losQuePertenecen xs s

--II

--0(n)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos xs = setToList (setList xs)

setList :: Eq a => [a] -> Set a
setList [] = emptyS
setList (x:xs) = addS x (setList xs)

--III

--0(1)
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos (NodeT s ti td) = unionS (unionS s (unirTodos ti)) (unirTodos td)

--EJERCICIO 3 

-- ====================
-- =       QUEUE      =
-- ====================

--I
--0(n)
lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if (isEmptyQ q)
            then 0
            else 1 + (lengthQ(dequeue q))

--II
--0(n)
queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola
queueToList xs = if isEmptyQ xs
                     then []
                     else [firstQ xs] ++ queueToList (dequeue xs)

--III
--0(1)
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2
               then q1
               else unionQ (enqueue (firstQ q2) q1) (dequeue q2) 

--EJERCICIO 4

-- ====================
-- =       STACK      =
-- ====================

--I
--0(1)
apilar :: [a] -> Stack a
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar [] = emptySS
apilar (x:xs) = push x (apilar xs)

--II
--0(n)
desapilar :: Stack a -> [a]
--Dada una pila devuelve una lista sin alterar el orden de los elementos
desapilar xs = if isEmptyS xs
                   then []
                   else top xs : desapilar (pop xs)

--III

insertarEnPos :: Int -> a -> Stack a -> Stack a
--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos 0 x stack = push x stack
insertarEnPos n x stack = insertarEnPos (n-1) x (pop stack)
