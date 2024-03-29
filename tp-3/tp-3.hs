{- PRACTICA 1 -}

data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show 

celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

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


-- a 

hayTesoro :: Camino -> Bool 
hayTesoro (Cofre os c) = hayTesoroEn os 
hayTesoro _            = False 

hayTesoroEn :: [Objeto] -> Bool 
hayTesoroEn []     = False 
hayTesoroEn (o:os) = esTesoro o || hayTesoroEn os  

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _      = False 

-- b 

pasosHastaTesoro :: Camino -> Int
-- Precond: Hay al menos un tesoro 
pasosHastaTesoro (Cofre os ca) = if hayTesoro os 
                                    then 0 -- Puede existir la posibilidad de que hayan mas tesoros en el camino
                                 else 1 + pasosHastaTesoro ca
pasosHastaTesoro (Nada ca)     = 1 + pasosHastaTesoro ca   