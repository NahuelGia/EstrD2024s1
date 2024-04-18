module Set2 
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where 

data Set a = S [a]  
          -- lista  

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


emptyS  = (S [] )

addS e (S ys) = (S (e:ys))

belongs e (S ys) = elem e ys

sizeS (S ys) = cantElementosSinRepetir ys

removeS e (S ys ) = (S (listaSin ys e)) -- Si el elemento no está devuelve el conjunto tal cual se encuentra 
           

unionS (S xs ) (S ys ) = (S (xs ++ ys))

setToList (S ys) = sinRepetidos ys 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = singularSi x (not (elem x xs)) ++ sinRepetidos xs 

singularSi :: a -> Bool -> [a]
singularSi e True  = [e]
singularSi _ False = []

cantElementosSinRepetir :: Eq a => [a] -> Int 
cantElementosSinRepetir []     = 0
cantElementosSinRepetir (x:xs) = unoSi (not (elem x xs)) + cantElementosSinRepetir xs 

unoSi :: Bool -> Int 
unoSi True  = 1
unoSi False = 0 

listaSin :: Eq a => [a] -> a -> [a]
listaSin []     _ = []
listaSin (x:xs) e = if e == x   
                    then xs 
                    else x : (listaSin xs e)

