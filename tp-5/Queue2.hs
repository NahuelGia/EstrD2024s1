module Queue2
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue )
where

data Queue a = Q [a] 


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

emptyQ = (Q [])

isEmptyQ (Q xs) = null xs  

enqueue e (Q xs) = (Q (e:xs))
 
firstQ (Q xs) = last xs

dequeue (Q xs) = (Q (init xs))

