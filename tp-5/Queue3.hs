module Queue3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue )
where

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


