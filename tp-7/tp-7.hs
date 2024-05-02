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
elMinimoMayorA e (NodeT e2 ti td) = if e < e2
                                    then minJust e2 (elMaximoMenorA e ti)
                                    else Nothing
