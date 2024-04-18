module Stack 
 (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = Stack [a] Int 
{-
INV.REP.: En Stack xs n 
* Cuando xs es vacia n es cero
* Cuando xs no es vacia n es la cantidad de elementos de xs
-}

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

emptyS = (Stack [] 0)

isEmptyS (Stack xs _) = null xs

push a (Stack xs n) = (Stack (a:xs)(n+1))

top (Stack xs _) = head xs 

pop (Stack xs n) =  (Stack (tail xs) (n-1))

lenS (Stack xs n) = n


