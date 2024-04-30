import PriorityQueue
import Map
import MultiSet

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


map1 = assocM "a" 10 (assocM "a" 1 (assocM "b" 2 (assocM "c" 3 emptyM)))

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

-- 5  

agruparEq :: Eq k => [(k, v)] -> Map k [v]   
agruparEq []         = emptyM
agruparEq ((k,v):xs) = agruparPorClave k v (agruparEq xs) 

agruparPorClave :: Eq k => k -> v -> Map k [v] -> Map k [v]
agruparPorClave k v map = case lookupM k map of 
                          Nothing  -> assocM k [v] map 
                          Just vs  -> assocM k (v:vs) map 
                                 
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

-- 7   

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps map1 map2 = asociarEnMap (keys map1) map1 map2 

asociarEnMap :: Eq k => [k] -> Map k v -> Map k v  -> Map k v 
asociarEnMap []     map1 map2 = map2 
asociarEnMap (k:ks) map1 map2 = assocM k (fromJust (lookupM k map1)) (asociarEnMap ks map1 map2)

{- EJERCICIO 5 -}

-- a 

indexar :: [a] -> Map Int a -- O(n^2)
indexar xs = indexarAPartirDeIndice 0 xs 

indexarAPartirDeIndice :: Int -> [a] ->  Map Int a -- O(n^2)
indexarAPartirDeIndice _ []     = emptyM 
indexarAPartirDeIndice n (x:xs) = assocM n x (indexarAPartirDeIndice (n+1) xs)

{-
 Se puede realizar sin usar el indice y 
 haciendo a indexar, una funcion por recursion 
-}

-- b

-- Se puede resolver construyendo un map directamente
-- sin una lista de por medio 

ocurrencias :: String -> Map Char Int 
ocurrencias []     = emptyM
ocurrencias (c:cs) = assocM c ( 1 + (apariciones c cs)) (ocurrencias cs)

apariciones :: Eq a => a -> [a] -> Int 
apariciones _ []     =  0 
apariciones e (x:xs) = unoSi (e == x) + apariciones e xs 

unoSi :: Bool -> Int 
unoSi True  = 1
unoSi False = 0 

{- EJERCICIO 6 -}

multiconjunto1 :: MultiSet Char
multiconjunto1 = addMS 'a' (addMS 'b' (addMS 'a' emptyMS))

multiconjunto2 :: MultiSet Char
multiconjunto2 = addMS 'a' (addMS 'c' (addMS 'd' emptyMS))

-- 2

ocurrencias' :: String -> MultiSet Char  
ocurrencias' []     = emptyMS
ocurrencias' (c:cs) = addMS c (ocurrencias' cs)

