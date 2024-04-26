import Set 
-- import Set2
-- import Queue 
import Queue3
import Stack

{- EJERCICIO 1 -}
{-
head' :: [a] -> a
head' (x:xs) = x
-- O(1)

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
-- O(1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- O(n) donde n es el numero sobre el que se hace recursion 

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
-- 0(n) donde n es la lista sobre la que se hace RE  

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
-- 0(n*m) La n por la RE sobre la lista dada y la  
-- m por factorial del numero   

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs 
-- Lineal 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
 if pertenece x xs
 then sinRepetidos xs
 else x : sinRepetidos xs
-- (n # Donde n es la lista ya que se hace RE sobre la misma 
--  *
--  n # Donde n es "pertenece" de una parte de la misma lista ) = (nxn) 

-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
-- Lineal 

concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
-- Cuadratico

takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
-- 0(n)
-- donde n es el numero, ya que se hace recursion sobre el 

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
-- O(n)
-- donde n es el numero, ya que se hace recursion sobre el   

partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
-- O(2n)
-- donde n es el numero, dado que se realizan dos operaciones de dicho costo 
-- de manera independiente  

minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
-- Lineal 

sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
 if n == x
 then xs
 else x : sacar n xs
-- O(n)
-- donde n es la cantidad de elementos de la lista 

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
 let m = minimo xs
 in m : ordenar (sacar m xs)
-- O(n # donde n es la longitud de la lista porque se realiza RE sobre la misma
-- *
-- (n  # por minimum sobre la lista 
--  +
-- n por sacar sobre la lista))
-- O (n *(n+n)) = (n*(2n)) = (nxn) Al multiplicar por un numero no afecta el costo por se constante 

-}
{- EJERCICIO 2 -}

setPrueba :: Set Int 
setPrueba = addS 3 (addS 2 (addS 1 Set.emptyS))

setPrueba2 :: Set Int 
setPrueba2 = addS 4 (addS 2 (addS 1 Set.emptyS))

-- 2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show 

-- a

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     _   = []
losQuePertenecen (x:xs) set = if belongs x set 
                              then x : (losQuePertenecen xs set) 
                              else losQuePertenecen xs set 

-- b 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos xs = setToList (agregarLista xs Set.emptyS)

agregarLista :: Eq a => [a] -> Set a -> Set a
agregarLista []     set = set 
agregarLista (x:xs) set = agregarLista xs (addS x set)

-- c 

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT          = Set.emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1)
                                             (unirTodos t2))  

{- Ejercicio 3 -}

-- 3 

queue1 = enqueue 6 (enqueue 5 (enqueue 4 (enqueue 3 (enqueue 2 (enqueue 1 (emptyQ))))))
queue2 = enqueue 12 (enqueue 11 (enqueue 10 (enqueue 9(enqueue 8 (enqueue 7 (emptyQ))))))
queue3 = enqueue 3 (enqueue 2 (enqueue 1 (enqueue 0 (emptyQ))))
queue4 = emptyQ



-- a

lengthQ :: Queue a -> Int
lengthQ q = if (isEmptyQ q ) 
            then 0 
            else 1 + (lengthQ (dequeue q))

-- b 

queueToList :: Queue a -> [a]
queueToList q = if (isEmptyQ q)
                then []
                else firstQ q  : queueToList (dequeue q) 

-- c 

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if (isEmptyQ q2)
               then q1
               else enqueue (firstQ q2) (unionQ q1 (dequeue q2)) 
-- No respeta el orden de q2, arreglar

{- EJERCICIO 4 -}

pila1 = push 1 (push 2 (push 3 Stack.emptyS))

-- a 

apilar :: [a] -> Stack a
apilar []     = Stack.emptyS
apilar (x:xs) = push x  (apilar xs) 

-- b 

desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s 
              then []
              else (top s) : desapilar (pop s)

-- c 

insertarEnPos :: Int -> a -> Stack a -> Stack a
-- Precond: El Stack tiene la una cantidad igual o mayor al numero dado 
insertarEnPos 0 e s = push e s 
insertarEnPos n e s = push (top s) (insertarEnPos (n-1) e (pop s))