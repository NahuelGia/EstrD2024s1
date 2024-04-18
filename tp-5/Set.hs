module Set 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where 

data Set a = S [a] Int 
          -- lista cantidad de elementos 
 {- 
    INV.REP.: en S xs n CONSULTAR 
    * xs no tiene repetidos  
    * si xs es vacía n es cero 
    * si xs no es vacía n es la cantidad de elementos de xs 
 -}


emptyS :: Set a 
-- Crea un conjunto vacio.
addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
sizeS :: Eq a => Set a -> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.
removeS :: Eq a => a -> Set a -> Set a
-- Borra un elemento del conjunto.
unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.


emptyS  = (S [] 0)

addS e (S ys n) = if elem e ys -- Se puede evitar recorrer toda la lista por cada uso?
                  then (S ys n)
                  else (S (e:ys) (n+1))        

belongs e (S ys _) = elem e ys

sizeS (S ys n) = n

removeS e (S ys n) = if elem e ys -- Se puede evitar recorrer toda la lista por cada uso?
                     then (S (listaSin ys e) (n-1))
                     else (S ys n)

unionS (S xs _) s = agregarListaASet xs s -- Hay una forma menos costosa?

setToList (S ys n) = ys   

agregarListaASet :: Eq a => [a] -> Set a -> Set a
agregarListaASet []     s = s 
agregarListaASet (x:xs) s = agregarListaASet xs (addS x s)


listaSin :: Eq a => [a] -> a -> [a]
-- Precond: El elemento se encuentra en la lista 
listaSin (x:xs) e = if e == x   
                    then xs 
                    else x : (listaSin xs e)

