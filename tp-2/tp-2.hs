{- EJERCICIO 1 -}

-- 1

sumatoria :: [Int] -> Int 
sumatoria []     = 0
sumatoria (n:ns) = n + (sumatoria ns)

-- 2

longitud :: [a] -> Int 
longitud []     = 0
longitud (x:xs) = 1 + (longitud xs)

-- 3 

sucesores :: [Int] -> [Int] 
sucesores []     = []
sucesores (n:ns) = (n+1) : sucesores ns    

-- 4

conjuncion :: [Bool] -> Bool 
conjuncion []     = True 
conjuncion (b:bs) = b && (conjuncion bs)

-- 5

disyuncion :: [Bool] -> Bool 
disyuncion []     = False
disyuncion (b:bs) = b || (disyuncion bs)

-- 6

aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:xs) = x ++ (aplanar xs)

-- 7

pertence :: Eq a => a -> [a] -> Bool 
pertence e []     = False  
pertence e (x:xs) = if (e == x)
                      then True 
                    else pertence e xs

-- 8 

apariciones :: Eq a => a -> [a] -> Int 
apariciones _ []     =  0 
apariciones e (x:xs) = if(e == x)
                        then 1 + (apariciones e xs)
                       else apariciones e xs

-- 9 

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []     = []
losMenoresA n (x:xs) = if (x<n)
                        then x : losMenoresA n xs 
                       else losMenoresA n xs  

-- 10 

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) = if((longitud x) > n)
                                then x : lasDeLongitudMayorA n xs  
                               else lasDeLongitudMayorA n xs 

-- 11 

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     e = [e]  
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e  

-- 12 

agregar :: [a] -> [a] -> [a]
agregar [] ys     = ys  
agregar (x:xs) ys = x : agregar xs ys 

-- 13

reversa :: [a] -> [a]   
reversa []     = []
reversa (x:xs) = reversa xs ++ [x] 

-- 14 

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []      ys     = ys 
zipMaximos xs      []     = xs 
zipMaximos (x:xs) (y:ys) = if (x > y)
                            then x : zipMaximos xs ys  
                           else  y : zipMaximos xs ys  

-- 15

elMinimo :: Ord a => [a] -> a 
-- Precond: La lista no puede ser vacia 
elMinimo [x]    = x 
elMinimo (x:xs) = if (x < (elMinimo xs))
                    then x
                  else elMinimo xs   

{- EJERCICIO 2 -}

-- 1

factorial :: Int -> Int
-- Precond: n es mayor o igual a 0
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 2

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1) 

-- 3 

repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e : repetir (n-1) e 

-- 4

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 xs     = []
losPrimeros n []     = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 