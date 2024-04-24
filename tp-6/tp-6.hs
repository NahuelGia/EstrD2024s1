import PriorityQueue
import Map 

queue1 =  (insertPQ 10(insertPQ 5(insertPQ 6(insertPQ 2(insertPQ 1 emptyPQ)))))

{- EJERCICIO 2 -}

heapSort :: Ord a => [a] -> [a] -- O(n + n^2) La primer en es por el costo de pqToList y la segunda por 
heapSort xs = pqToList (listToPQ xs)       -- el costo de listToPQ

listToPQ :: Ord a => [a] -> PriorityQueue a -- O(n^2) Donde una n es la lista sobre la que se hace RE
listToPQ []     = emptyPQ                          -- y la otra es por insertPQ
listToPQ (x:xs) = insertPQ x (listToPQ xs) 

pqToList :: Ord a => PriorityQueue a -> [a] --  O(n) Donde n es la PQ sobre la que se hace RE.
pqToList pq = if isEmptyPQ pq 
              then []
              else (findMinPQ pq) : (pqToList (deleteMinPQ pq))

{- EJERCICIO 3 -}

-- 1 

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = valoresPorKey map (keys map)

valoresPorKey :: Eq k => Map k v -> [k] -> [Maybe v] 
valoresPorKey map []     = []
valoresPorKey map (k:ks) = lookupM k map : (valoresPorKey map ks) 

-- 2 

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas []     map = True 
todasAsociadas (k:ks) map = if esNothing (lookupM k map) 
                            then False 
                            else True && (todasAsociadas ks map)

esNothing :: Maybe a -> Bool 
esNothing Nothing = True 
esNothing _       = False

-- 3 

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap []     = emptyM
listToMap (x:xs) = assocM (fst x) (snd x) (listToMap xs) 
                        --   k      v 

-- 4 

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = asociarKeysAValorEn (keys map) map

asociarKeysAValorEn :: Eq k => [k] -> Map k v -> [(k,v)] 
-- Precond: Todas las keys pertenecen al map.
asociarKeysAValorEn []     map = []
asociarKeysAValorEn (k:ks) map = (k, fromJust (lookupM k map)) : (asociarKeysAValorEn ks map)

fromJust :: Maybe a -> a -- O(1)
fromJust Nothing  = error "No hay elemento"
fromJust (Just a) = a

-- 5 PREGUNTAR  

agruparEq :: Eq k => [(k, v)] -> Map k [v]     -- 
agruparEq xs = listToMap (agruparPorClave xs)

agruparPorClave :: Eq k => [(k,v)] -> [(k,[v])] -- O(n^2) Donde una n es la lista sobre la que se hace RE 
agruparPorClave []     = []                            -- y la otra n es por agruparPorClave
agruparPorClave (x:xs) = insertarElemento x (agruparPorClave xs) 

insertarElemento :: Eq k => (k,v) -> [(k,[v])] -> [(k,[v])] -- O(n) Donde n es la lista sobre la que se hace RE
insertarElemento (k,v) []           = [(k,[v])]
insertarElemento (k,v) ((k2,v2):xs) = if k == k2 
                                      then (k, v : v2) : xs 
                                      else (k2,v2) : (insertarElemento (k,v) xs)
                                 
-- 6 

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar []     map = map 
incrementar (k:ks) map = incrementarKeyEnMap k map 

incrementarKeyEnMap :: Eq k => k -> Map k Int -> Map k Int 
incrementarKeyEnMap k map =  let 
                             valorK = lookupM k map 
                             in 
                             if valorK == Nothing 
                             then map 
                             else assocM k  ((fromJust valorK) + 1) map

-- 7  PREGUNTAR 

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps map1 map2 = asociarEnMap (mapToList map1) map2 

asociarEnMap :: Eq k => [(k,v)] -> Map k v -> Map k v 
asociarEnMap []         map = map 
asociarEnMap ((k,v):xs) map = assocM k v (asociarEnMap xs map)

