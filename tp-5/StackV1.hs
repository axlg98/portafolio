module StackV1 
 (Stack, emptySS,isEmptyS,push,top,pop,lenS,st1)
where

data Stack a = S [a] deriving Show

st1 :: Stack Int
st1 = S [1,5,2,7]

--0(1)
emptySS :: Stack a
--Crea una pila vacía.
emptySS = S []

--0(1)
isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
isEmptyS (S xs) = null xs

--0(n)
push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
push x (S ys) = S(x:ys)

--0(1)
top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
-- PREC= *La lista del stack dado tiene que tener al menos un elemento.
top (S xs) = head xs

--0(1)
pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
--PREC. = * La lista tiene que tener al meno 2 elementos en la lista del stack dadoñ
pop (S xs) = S(tail xs)

--0(n)
lenS :: Stack a -> Int
--Dada la cantidad de elementos en la pila.
lenS (S xs) = length xs