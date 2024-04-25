module Map2
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = Map [(k,v)]

emptyM :: Map k v
-- Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
keys :: Eq k => Map k v -> [k]
-- Propósito: devuelve las claves del map.

emptyM = Map []

assocM k v (Map xs) =  Map ((k,v):xs)  -- O(1) 

lookupM k (Map xs) = valorEnLista k xs -- O(n) Donde n es por valorEnLista

deleteM k (Map xs) =  Map (borrarEnLista k xs) -- O(n) Donde n es por borrarEnLista

keys (Map xs) = sinRepetidos (keysEnLista xs)  -- O(2n) Donde n es por keysEnLista y la otra n es por sinRepetidos
                -- Preguntar 

valorEnLista :: Eq k => k -> [(k,v)] -> Maybe v  -- O(n) Donde n es la lista sobre la que se hace RE.
valorEnLista k []          = Nothing
valorEnLista k ((k2,v):xs) = if k == k2 
                             then Just v 
                             else valorEnLista k xs 

borrarEnLista :: Eq k => k -> [(k,v)] -> [(k,v)] -- O(n) Donde n es la lista sobre la que se hace RE.
borrarEnLista k []          = []
borrarEnLista k ((k2,v):xs) = if k == k2 
                              then borrarEnLista k xs  
                              else (k2,v) : (borrarEnLista k xs) 

keysEnLista :: [(k,v)] -> [k] -- O(n) Donde n es la lista sobre la que se hace RE.
keysEnLista []         = []
keysEnLista ((k,v):xs) = k : (keysEnLista xs) 

sinRepetidos :: Eq k => [k] -> [k] -- O(n) Donde n es la lista sobre la que se hace RE.
sinRepetidos []     = []
sinRepetidos (k:ks) = if elem k ks 
                      then sinRepetidos ks 
                      else k : (sinRepetidos ks)