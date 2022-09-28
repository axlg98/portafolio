module PriorityQueue (PrioQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PrioQueue a = PQ [a] 

--0(1)
emptyPQ :: PrioQueue a
--Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

--0(1)
isEmptyPQ :: PrioQueue a -> Bool                            
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

--0(1)
insertPQ :: Ord a => a -> PrioQueue a -> PrioQueue a
--Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ ys) =PQ(x:ys)

--0(n)
findMinPQ :: Ord a => PrioQueue a -> a
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = minimum xs

--0(n)
deleteMinPQ :: Ord a => PrioQueue a -> PrioQueue a
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = (PQ (borrarElMin xs))

--0(n)
borrarElMin :: Ord a => [a] -> [a]
--PRECOND.= *La lista no puede ser vacía.
borrarElMin (xs) = borrar (minimum xs) xs

--0(n)
borrar :: Eq a => a -> [a] -> [a]
borrar x (y:ys) = if x==y
                 then ys
                 else y : borrar x ys


