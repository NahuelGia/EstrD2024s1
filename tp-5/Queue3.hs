module Queue3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue )
where
{-
INV.REP.: En Q fs bs
* Cuando fs es vacia, bs tambien es vacia 
-}

data Queue a = Q [a] [a]

emptyQ :: Queue a
-- Crea una cola vacía.
isEmptyQ :: Queue a -> Bool
-- Dada una cola indica si la cola está vacía.
enqueue :: a -> Queue a -> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a
-- Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a
-- Dada una cola la devuelve sin su primer elemento.

emptyQ = Q [] []

isEmptyQ (Q fs _) = null fs 

enqueue e (Q fs bs) = if null fs -- O(n)
                      then (Q [e] bs)
                      else (Q fs (e:bs)) 

firstQ (Q fs _)   = head fs -- O(1)

dequeue (Q fs bs) = case tail fs of 
                    [] -> Q (reverse bs) []
                    xs -> Q xs bs
                    
-- O (
-- n # la cantidad de elementos de bs por reverse 
-- )


