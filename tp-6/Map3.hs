module Map3
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = Map [k] [v]
{-
INV.REP.: En Map ks vs 
* ks y vs tienen la misma longitud .
-}

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

emptyM = Map [] [] -- O(1)

assocM k v (Map ks vs) = Map (k:ks) (v:vs)  -- O(1) 

lookupM k (Map ks vs) = valorEnLista k ks vs -- O(2n) Donde n es por valoEnLista

deleteM k (Map ks vs) = mapSinKey k ks vs -- O(2n) Donde n es por mapSinKey

keys (Map ks vs) = sinRepetidos ks  -- O(n) 

mapSinKey :: Eq k => k -> [k] -> [v] -> Map k v -- O(2n)
mapSinKey k []      _      = emptyM
mapSinKey k (k2:ks) (v:vs) = if k == k2 
                             then Map (ks) (vs)
                             else assocM k2 v (mapSinKey k ks vs)

valorEnLista :: Eq k => k -> [k] -> [v] -> Maybe v -- O(2n) Donde una n es la lista de keys sobre la que se hace RE 
valorEnLista k []      _      = Nothing        -- y la otra es la lista de values .
valorEnLista k (k2:ks) (v:vs) = if k == k2 
                                then Just v 
                                else valorEnLista k ks vs 

sinRepetidos :: Eq k => [k] -> [k] -- O(n) Donde n es la lista sobre la que se hace RE.
sinRepetidos []     = []
sinRepetidos (k:ks) = if elem k ks 
                      then sinRepetidos ks 
                      else k : (sinRepetidos ks)