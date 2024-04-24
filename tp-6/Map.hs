module Map 
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = Map [(k,v)]
{-
INV.REP.: La lista del map no tiene claves repetidas 
-}
emptyM :: Map k v
-- Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
keys :: Map k v -> [k]
-- Propósito: devuelve las claves del map.

emptyM = Map []

assocM k v (Map xs) =  Map (asociarALista k v xs)  -- O(n) Donde n es por asociarALista sobre xs 

lookupM k (Map xs) = valorEnLista k xs -- O(n) Donde n es por valoEnLista

deleteM k (Map xs) =  Map (borrarEnLista k xs) -- O(n) Donde n es por borrarEnLista

keys (Map xs) = keysEnLista xs  -- O(n) Donde n es por keysEnLista

asociarALista :: Eq k => k -> v -> [(k,v)] -> [(k,v)] -- O(n) Donde n es la lista sobre la que se hace RE.
asociarALista k v []           = [(k,v)]
asociarALista k v ((k2,v2):xs) = if k == k2 
                                 then (k,v) : xs -- Reemplazo el valor  
                                 else (k2,v2) : (asociarALista k v xs)  


valorEnLista :: Eq k => k -> [(k,v)] -> Maybe v  -- O(n) Donde n es la lista sobre la que se hace RE.
valorEnLista k []          = Nothing
valorEnLista k ((k2,v):xs) = if k == k2 
                             then Just v 
                             else valorEnLista k xs 

borrarEnLista :: Eq k => k -> [(k,v)] -> [(k,v)] -- O(n) Donde n es la lista sobre la que se hace RE.
borrarEnLista k []          = []
borrarEnLista k ((k2,v):xs) = if k == k2 
                              then xs 
                              else (k2,v) : (borrarEnLista k xs) 

keysEnLista :: [(k,v)] -> [k] -- O(n) Donde n es la lista sobre la que se hace RE.
keysEnLista []         = []
keysEnLista ((k,v):xs) = k : (keysEnLista xs) 