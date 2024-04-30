{- EJERCICIO 2 -}

-- 1

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST e (Tree e2 EmptyT EmptyT) = e == e2 
belongsBST e (Tree e2 ti td)         = if e == e2 
                                       then True 
                                       else if e > e2 
                                            then belongsBST e td 
                                            else belongsBST e ti  

-- 2 







