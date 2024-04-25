module MultiSet 
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import Map 

data MultiSet a = MS (Map a Int)
-- Preguntar Inv.

emptyMS :: MultiSet a
-- Propósito: denota un multiconjunto vacío.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
multiSetToList :: Ord a => MultiSet a -> [(a, Int)]
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.

emptyMS = MS emptyM

addMS e (MS map) = MS (agregarOcurrencia e map)

ocurrencesMS e (MS map) = ocurrenciasDe e map

unionMS (MS map) (MS map2) = (MS (mergeMaps map map2) )

intersectionMS (MS map) (MS map2) = (MS (interseccionListaMap (mapToList map) map2))

multiSetToList (MS map) = mapToList map

interseccionListaMap :: Eq k => [(k,Int)] -> Map k Int -> Map k Int
interseccionListaMap []         map = emptyM
interseccionListaMap ((k,v):xs) map = case lookupM k map of 
                                      Nothing -> interseccionListaMap xs map 
                                      Just v2  -> assocM k (v+v2) (interseccionListaMap xs map) 

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps map1 map2 = asociarEnMap (mapToList map1) map2 

asociarEnMap :: Eq k => [(k,v)] -> Map k v -> Map k v 
asociarEnMap []         map = map 
asociarEnMap ((k,v):xs) map = assocM k v (asociarEnMap xs map)

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = asociarKeysAValorEn (keys map) map

asociarKeysAValorEn :: Eq k => [k] -> Map k v -> [(k,v)] 
-- Precond: Todas las keys pertenecen al map.
asociarKeysAValorEn []     map = []
asociarKeysAValorEn (k:ks) map = (k, fromJust (lookupM k map)) : (asociarKeysAValorEn ks map)

fromJust :: Maybe a -> a -- O(1)
fromJust Nothing  = error "No hay elemento"
fromJust (Just a) = a

ocurrenciasDe :: Ord a => a -> Map a Int -> Int 
ocurrenciasDe a map = case lookupM a map of 
                      Nothing -> 0
                      Just v  -> v

agregarOcurrencia :: Ord a => a -> Map a Int -> Map a Int
agregarOcurrencia a map = case  lookupM a map  of 
                          Nothing -> assocM a 1 map  
                          Just v  -> assocM a (v + 1) map 


