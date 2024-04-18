module Stack 
 (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = Stack [a]

emptyS :: Stack a
-- Crea una pila vacía.
isEmptyS :: Stack a -> Bool
-- Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a
-- Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a
-- Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a
-- Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int
-- Da la cantidad de elementos en la pila.
--Costo: constante.

emptyS = (Stack [])

isEmptyS (Stack xs) = null xs

push a (Stack xs) = (Stack (a:xs))

top (Stack xs) = head xs 

pop (Stack xs) = (Stack (tail xs))

lenS (Stack xs) = length xs


