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

-- 1 
