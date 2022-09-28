module QueueDosListas
(Queue,emptyQ,isEmptyQ,enqueue,firstQ,dequeue,qq1,qq2)
where

data Queue a = QQ [a] [a] deriving Show
               -- fs  bs
{- INV. REP. = *Si fs es vacía, entonces bs también lo es.
-}

qq1 :: Queue Int 
qq1 = QQ [4,3,2,1] [6,4,2,9]

qq2 :: Queue Int
qq2 = QQ [2,1,5] [8,10,102]

--0(1)
emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = QQ [][]

--0(1)
isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (QQ fs bs) = null fs

--0(n)
enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (QQ fs bs) = QQ fs ((agregarAlFinal bs ) x)

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] k = [k]
agregarAlFinal (x:xs) k = x : agregarAlFinal xs k

--0(1)
firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
--PRECOND= * La lista del Queue tiene que tener al menos 1 elemento.
firstQ (QQ fs bs) = head fs

--0(1)
dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
--PRECOND= * La lista del Queue no puede ser vacía.
dequeue (QQ fs bs) = ordenarLaCola(QQ(tail fs) bs)

ordenarLaCola :: Queue a -> Queue a
ordenarLaCola (QQ fs bs) = if null fs
                           then QQ (reversa (bs)) []
                           else QQ fs bs

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs)  x