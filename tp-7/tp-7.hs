{- EJERCICIO 2 -}

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
   deriving (Show, Eq)

insertarNumeros :: Ord a => [a] -> Tree a -> Tree a
insertarNumeros [] tree = tree
insertarNumeros (x:xs) tree = insertarNumeros xs (insertBST x tree)

-- 1 

-- Es O(log N) porque solo se recorre una parte del arbol y por
--  cada vez que se llama a la recursión se divide en dos la cantidad 
--  de elementos que hay que recorrer gracias a la comparación por tamaño

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST e EmptyT          = False
belongsBST e (NodeT e2 ti td) = if e == e2 
                               then True 
                               else if e > e2 
                                    then belongsBST e td 
                                    else belongsBST e ti  

-- 2 

insertBST :: Ord a => a -> Tree a -> Tree a
insertBST e EmptyT          =  (NodeT e EmptyT EmptyT)
insertBST e (NodeT e2 ti td) =  if e == e2 
                               then (NodeT e ti td)
                               else if e > e2 
                                    then (NodeT e2 ti (insertBST e td))
                                    else (NodeT e2 (insertBST e ti) td)

-- 3                          

deleteBST :: Ord a => a -> Tree a -> Tree a
-- El arbol es BST
deleteBST e EmptyT           = EmptyT  
deleteBST e (NodeT e2 ti td) = if e == e2 
                               then rearmarBST ti td
                               else if e > e2 
                                    then NodeT e2 ti (deleteBST e td)
                                    else NodeT e2 (deleteBST e ti) td


rearmarBST :: Ord a => Tree a -> Tree a -> Tree a 
-- Los arboles son BST
rearmarBST ti EmptyT = ti 
rearmarBST ti td     = let (m, td') = splitMinBST td
                       in NodeT m ti td'

-- 4

splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- El arbol es BST y no está vacío
splitMinBST (NodeT e EmptyT td) = (e, td)
splitMinBST (NodeT e ti     td) = let (m, ti') = splitMinBST ti
                                  in (m, NodeT e ti' td)  

-- 5 

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- El arbol es BST y no está vacío
splitMaxBST (NodeT e ti EmptyT) = (e, ti)
splitMaxBST (NodeT e ti     td) = let (m, td') = splitMaxBST td
                                  in (m, NodeT e ti td') 

-- 6 

esBST :: Ord a => Tree a -> Bool -- O(n*(n+n)) = (n*2n) = (n^2) .
esBST EmptyT          = True 
esBST (NodeT e ti td) = (losElementosSonMenores e ti) && (losElementosSonMayores e td) && (esBST ti) && (esBST td)

losElementosSonMayores :: Ord a => a -> Tree a -> Bool -- O(n) donde N es el arbol sobre el que se hace RE .
losElementosSonMayores x EmptyT          = True 
losElementosSonMayores x (NodeT e ti td) = (e > x) && (losElementosSonMayores x ti) && (losElementosSonMayores x td)

losElementosSonMenores :: Ord a => a -> Tree a -> Bool -- O(n) donde N es el arbol sobre el que se hace RE . 
losElementosSonMenores x EmptyT          = True 
losElementosSonMenores x (NodeT e ti td) = (e < x) && (losElementosSonMenores x ti) && (losElementosSonMenores x td)

-- 7 

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a -- Preguntar 
elMaximoMenorA e EmptyT           = Nothing 
elMaximoMenorA e (NodeT e2 ti td) = if e > e2
                                    then maxJust e2 (elMaximoMenorA e td)
                                    else elMaximoMenorA e ti 

maxJust :: Ord a => a -> Maybe a -> Maybe a 
maxJust e Nothing   = Just e  
maxJust e (Just e2) = Just (max e e2)

-- 8

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA e EmptyT           = Nothing 
elMinimoMayorA e (NodeT e2 ti td) = if e >= e2  
                                    then elMinimoMayorA e td  
                                    else minJust e2 (elMinimoMayorA e ti) -- e < e2 

minJust :: Ord a => a -> Maybe a -> Maybe a -- O(1)
minJust e Nothing   = Just e 
minJust e (Just e2) = Just (min e e2)

-- 9 

balanceado :: Tree a -> Bool -- O(n*n) Por RE sobre el Tree y por balanceado 
balanceado EmptyT          = True 
balanceado (NodeT _ ti td) = abs ((heightT ti) - (heightT td)) <= 1 
                              && balanceado ti && balanceado td

heightT :: Tree a -> Int -- O(n) por RE sobre el Tree 
heightT EmptyT          = 0 
heightT (NodeT _ ti td) = 1 + max (heightT ti) (heightT td) 

{- EJERCICIO 5 -}

-- a

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
comenzarCon []       xs = comenzarConEmpleados xs
comenzarCon (id:ids) xs = agregarSector id (comenzarCon ids xs)  

comenzarConEmpleados :: [CUIL] -> Empresa 
comenzarConEmpleados []     = consEmpresa
comenzarConEmpleados (c:cs) = agregarEmpleado c (comenzarConEmpleados cs)

-- b 

recorteDePersonal :: Empresa -> Empresa
recorteDePersonal empresa = let 
                            empleados    = todosLosCUIL empresa 
                            nroEmpleados = length empleados
                            in
                            borrarEmpleados empresa ( take (round (nroEmpleados/2)) empleados )

borrarEmpleados :: Empresa -> [CUIL] -> Empresa 
borrarEmpleados e  []     = e
borrarEmpleados e  (c:cs) = borrarEmpleado c (borrarEmpleados e cs )

-- c 

convertirEnComodin :: CUIL -> Empresa -> Empresa
convertirEnComodin cuil empresa =  agregarASectores (todosLosSectores empresa) cuil empresa

agregarASectores :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarASectores []     cuil empresa =  empresa
agregarASectores (x:xs) cuil empresa =  agregarASector x cuil (agregarASectores xs cuil consEmpresa)

-- d 

esComodin :: CUIL -> Empresa -> Bool
esComodin cuil empresa = seEncuentraEn (buscarPorCUIL cuil empresa) (todosLosSectores empresa) empresa

seEncuentraEn :: Empleado -> [SectorId] -> Empresa
seEncuentraEn empleado []     empresa = True 
seEncuentraEn empleado (x:xs) empresa = elem empleado (empleadosDelSector x empresa) 
                                        && seEncuentraEn empleado xs empresa