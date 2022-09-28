module QueueV2
  (Queue,emptyQ,isEmptyQ,enqueue,firstQ,dequeue,q1,q2)
where
data Queue a = Q [a] deriving Show

--AGREGA POR ADELANTE Y SACA POR ATRÁS

q1 :: Queue Int 
q1 = Q [4,3,2,1,1]

q2 :: Queue Int
q2 = Q [2,1,5]

--0(1)
emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = Q []

--0(1)
isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs

--0(n)
enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q ys) = Q (x:ys)

--0(1)
firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
--PRECOND= * La lista del Queue tiene que tener al menos 1 elemento.
firstQ (Q xs) = head xs

--0(1)
dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
--PRECOND= * La lista del Queue no puede ser vacía.
dequeue (Q xs) = Q(tail xs)