import PriorityQueue
import MapV1
--0(n)
heapSort :: Ord a => [a] -> [a]
heapSort xs = heapList(insertar xs)

heapList :: Ord a => PrioQueue a -> [a]
heapList xs = if isEmptyPQ xs
              then []
              else findMinPQ xs:(heapList(deleteMinPQ xs))

insertar :: Ord a => [a] -> PrioQueue a
insertar [] = emptyPQ
insertar (x:xs) = insertPQ x (insertar xs )


instance (Show a,Ord a) => Show (PrioQueue a) where 
    show pq = if isEmptyPQ pq 
             then " ||"
             else " << " ++ show (findMinPQ pq) ++ show (deleteMinPQ pq)

--------------------------------------------
-- ====================
-- =         MAP      =
-- ====================

--EJERCICIO 3

--I

valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM map = valoresDelMaybe (keys map) map

valoresDelMaybe :: Eq k => [k] -> Map k v -> [Maybe v]
valoresDelMaybe []     map = []
valoresDelMaybe (k:ks) map = lookupM k map : valoresDelMaybe ks map

--II

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas
todasAsociadas []     map = True
todasAsociadas (k:ks) map = case lookupM k map of
                              Just v -> True && todasAsociadas ks map
                              Nothing -> False