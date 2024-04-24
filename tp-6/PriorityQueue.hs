module PriorityQueue
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where 

data PriorityQueue a = Pq [a]

emptyPQ :: PriorityQueue a
-- Propósito: devuelve una priority queue vacía.
isEmptyPQ :: PriorityQueue a -> Bool
-- Propósito: indica si la priority queue está vacía.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
-- Propósito: inserta un elemento en la priority queue.
findMinPQ :: Ord a => PriorityQueue a -> a
-- Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
-- Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.


emptyPQ = Pq [] -- O(1)

isEmptyPQ (Pq xs) = null xs -- O(1) 

insertPQ e (Pq xs) = (Pq (insertarEnOrden e xs) ) -- O(n) Por el costo de insertarEnOrden 

findMinPQ (Pq xs)  =  head xs -- O(1)

deleteMinPQ (Pq xs) = (Pq (tail xs)) -- O(1)


insertarEnOrden :: Ord a => a -> [a] -> [a] -- O(n) Donde n es la lista sobre la que se hace RE.
insertarEnOrden e []     = [e]
insertarEnOrden e (x:xs) = if e <= x 
                           then e:x:xs 
                           else x : (insertarEnOrden e xs)