{- PRACTICA 1 -}

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show 

celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
celda2 = Bolita Rojo (Bolita Rojo CeldaVacia)

-- 1

-- a 

nroBolitas :: Color -> Celda -> Int
nroBolitas _   CeldaVacia      =  0  
nroBolitas co1 (Bolita co2 ce) =  unoSi (sonElMismoColor co1 co2) + nroBolitas co1 ce

sonElMismoColor :: Color -> Color -> Bool
sonElMismoColor Azul Azul = True 
sonElMismoColor Rojo Rojo = True 
sonElMismoColor _    _    = False 

unoSi :: Bool -> Int 
unoSi True  = 1
unoSi False = 0 


-- b 

poner :: Color -> Celda -> Celda 
poner co ce = Bolita co ce  

-- c 

sacar :: Color -> Celda -> Celda 
sacar _   CeldaVacia      = CeldaVacia
sacar co1 (Bolita co2 ce) = if (sonElMismoColor co1 co2)
                             then ce
                            else (Bolita co2 (sacar co1 ce))  

-- d 

ponerN :: Int -> Color -> Celda -> Celda 
ponerN 0 _  ce = ce  
ponerN n co ce = ponerN (n-1) co (poner co ce)

-- 2 

data Objeto = Cacharro | Tesoro
    deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show

camino1 = Cofre [Cacharro, Tesoro] (Cofre [Tesoro] (Cofre [Cacharro] Fin))
camino2 = Cofre [Cacharro, Cacharro] Fin
camino3 = Cofre [Cacharro, Cacharro] (Cofre [Tesoro] (Cofre [Cacharro] Fin))


-- a 

hayTesoro :: Camino -> Bool 
hayTesoro (Cofre os c) = tieneTesoro os 
hayTesoro _            = False 

tieneTesoro :: [Objeto] -> Bool 
tieneTesoro []     = False 
tieneTesoro (o:os) = esTesoro o || tieneTesoro os  

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _      = False 

-- b 

pasosHastaTesoro :: Camino -> Int
-- Precond: Hay al menos un tesoro 
pasosHastaTesoro (Cofre os ca) = if tieneTesoro os 
                                 then 0 -- Puede existir la posibilidad de que hayan mas tesoros en el camino
                                 else 1 + pasosHastaTesoro ca
pasosHastaTesoro (Nada ca)     = 1 + pasosHastaTesoro ca  

-- pasosHastaTesoro ca = if hayTesoro ca then 0 else 1 + pasosHastaTesoro (caminoSiguiente ca) 

-- c 

hayTesoroEn :: Int -> Camino -> Bool 
hayTesoroEn _ Fin = False 
hayTesoroEn 0 ca  = hayTesoro ca 
hayTesoroEn n ca  = hayTesoroEn (n-1) (caminoSiguiente ca) 

caminoSiguiente :: Camino -> Camino 
-- Precondicion: El camino no puede ser Fin
caminoSiguiente (Cofre os ca) = ca 
caminoSiguiente (Nada ca)     = ca 

-- d

alMenosNTesoros :: Int -> Camino -> Bool 
alMenosNTesoros _ Fin = False  
alMenosNTesoros 0 _   = True 
alMenosNTesoros n ca  = if hayTesoro ca 
                         then alMenosNTesoros (n-1) (caminoSiguiente ca)
                        else alMenosNTesoros n (caminoSiguiente ca) 

-- e 

cantTesorosEntre :: Int -> Int -> Camino -> Int
-- Precond: El segundo numero es igual o mayor que el primero 
cantTesorosEntre _  _  Fin = 0 
cantTesorosEntre 0  0  ca  = unoSi (hayTesoro ca) 
cantTesorosEntre 0  n2 ca  = unoSi (hayTesoro ca) + cantTesorosEntre 0 (n2-1) (caminoSiguiente ca)
cantTesorosEntre n1 n2 ca  = cantTesorosEntre (n1-1) (n2-1) (caminoSiguiente ca) 

{- EJERCICIO 2 -}


-- 2.1

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show 

arbol1 :: Tree Int 
arbol1  =  NodeT 1
        (NodeT 2
            (NodeT 4 EmptyT EmptyT)
            (NodeT 5 EmptyT EmptyT)
        )
        (NodeT 3
            (NodeT 6 EmptyT EmptyT)
            (NodeT 7 EmptyT EmptyT)
        )


-- 1 

sumarT :: Tree Int -> Int
sumarT EmptyT        =  0 
sumarT (NodeT n x y) =  n + (sumarT x) + (sumarT y)

-- 2

sizeT :: Tree a -> Int 
sizeT EmptyT        = 0
sizeT (NodeT _ x y) = 1 + (sizeT x) + (sizeT y)

-- 3 

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT        = EmptyT
mapDobleT (NodeT n x y) = (NodeT (n*2) (mapDobleT x) (mapDobleT y))

-- 4 

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT           = False 
perteneceT e1 (NodeT e2 x y ) = (e1 == e2) || (perteneceT e1 x) || (perteneceT e1 y)

-- 5 

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT           = 0 
aparicionesT e1 (NodeT e2 x y ) = unoSi (e1 == e2) + (aparicionesT e1 x) + (aparicionesT e1 y)

-- 6 

leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT e EmptyT EmptyT) = [e] 
leaves (NodeT e x y)           = (leaves x) ++ (leaves y)

-- 7 

heightT :: Tree a -> Int 
heightT EmptyT                  = 0 
heightT (NodeT a EmptyT EmptyT) = 0
heightT (NodeT _ x y)           = 1 + max (heightT x) (heightT y)


-- 8 

mirrorT :: Tree a -> Tree a
mirrorT EmptyT        = EmptyT
mirrorT (NodeT e x y) = (NodeT e (mirrorT y) (mirrorT x) )

-- 9 

toList :: Tree a -> [a]
toList EmptyT        =  []
toList (NodeT e x y) = (toList x) ++ e : (toList y)

-- 10 

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT         = []
levelN 0 (NodeT e x y)  = [e]
levelN n (NodeT e x y)  = (levelN (n-1) x)  ++ (levelN (n-1) y)             

-- 11

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT        = []
listPerLevel (NodeT e x y) = [[e]] ++ agruparPorNivel (listPerLevel x) (listPerLevel y)

agruparPorNivel :: [[a]] -> [[a]] -> [[a]]
agruparPorNivel [] []         = []
agruparPorNivel xs []         = xs
agruparPorNivel [] ys         = ys  
agruparPorNivel (x:xs) (y:ys) = [x ++ y] ++ (agruparPorNivel xs ys)

-- 12

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT        = []
ramaMasLarga (NodeT e x y) = toList(laRamaMasLarga x y)

laRamaMasLarga :: Tree a -> Tree a -> Tree a 
laRamaMasLarga t1 t2 = if heightT t1 > heightT t2
                       then t1
                       else t2

-- 13 

consACada :: a -> [[a]] -> [[a]]
consACada e []       = []
consACada e (xs:xss) = (e:xs) : consACada e xss 

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT        = [] 
todosLosCaminos (NodeT e x y) = [e] : consACada e (todosLosCaminos x)
                             ++ consACada e (todosLosCaminos y)

-- 2 

data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA
    deriving Show

expresion1 :: ExpA
expresion1 = Prod (Sum (Valor 2) (Valor 3)) (Neg (Valor 5))

-- 1 

eval :: ExpA -> Int 
eval (Valor n)       = n 
eval (Sum  exA exB)  = (eval exA) + (eval exB)
eval (Prod exA exB)  = (eval exA) * (eval exB)
eval (Neg  ex)       = - (eval ex)  

-- 2 

simplificar :: ExpA -> ExpA
simplificar (Sum (Valor 0) x)  = x 
simplificar (Sum x (Valor 0))  = x
simplificar (Prod (Valor 0) x) = Valor 0 
simplificar (Prod x (Valor 0)) = Valor 0
simplificar (Prod (Valor 1) x) = x
simplificar (Prod x (Valor 1)) = x
simplificar (Neg (Neg x) )     = x
simplificar x                  = x


