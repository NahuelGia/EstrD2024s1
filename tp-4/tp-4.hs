singularSi :: a -> Bool -> [a]
singularSi e True  = [e]
singularSi _ False = []


{- EJERCICIO 1 -}

data Pizza = Prepizza
           | Capa Ingrediente Pizza
    deriving Show 

data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int
    deriving Show 

pizza1 :: Pizza
pizza1 = Capa Queso (Capa Jamon (Capa (Aceitunas 5) Prepizza))

-- a 

cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

-- b 

armarPizza :: [Ingrediente] -> Pizza 
armarPizza []     = Prepizza
armarPizza (i:is) = (Capa i (armarPizza is))

-- c 

sacarJamon :: Pizza -> Pizza 
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if esJamon i 
                        then sacarJamon p 
                        else (Capa i (sacarJamon p))

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True 
esJamon _     = False 

-- d 

tieneSoloSalsaYQueso :: Pizza -> Bool 
tieneSoloSalsaYQueso Prepizza   = True 
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p 

esSalsaOQueso :: Ingrediente -> Bool 
esSalsaOQueso Queso = True 
esSalsaOQueso Salsa = True 
esSalsaOQueso _     = False 

-- e

duplicarAceitunas :: Pizza -> Pizza 
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = (Capa (duplicarSiEsAceituna i) (duplicarAceitunas p))

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
duplicarSiEsAceituna (Aceitunas n) = (Aceitunas (n*2))
duplicarSiEsAceituna i             = i

-- f

cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps

{- EJERCICIO 2 -}

data Dir = Izq | Der
    deriving Show 

data Objeto = Tesoro | Chatarra
    deriving Show 

data Cofre = Cofre [Objeto]
    deriving Show 

data Mapa = Fin Cofre
          | Bifurcacion Cofre Mapa Mapa
    deriving Show 

mapa1 :: Mapa
mapa1 =
    Bifurcacion (Cofre [Tesoro, Chatarra])  
                (Fin (Cofre [Chatarra]))      
                (Fin (Cofre [Tesoro])) 

mapa2 :: Mapa
mapa2 =
    Bifurcacion (Cofre [Chatarra, Chatarra])  
                (Fin (Cofre [Chatarra]))      
                (Fin (Cofre [Chatarra]))  

mapaLargo :: Mapa
mapaLargo =
    Bifurcacion (Cofre [Tesoro])          
                (Bifurcacion (Cofre [Chatarra])  
                             (Fin (Cofre [Tesoro])) 
                             (Fin (Cofre [Tesoro])))               
                (Fin (Cofre [Chatarra]))     

-- 1 

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = tieneTesoro c 
hayTesoro (Bifurcacion c mi md) = tieneTesoro c || hayTesoro mi || hayTesoro md

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre os) = algunoEsTesoro os 

algunoEsTesoro :: [Objeto] -> Bool
algunoEsTesoro []     = False
algunoEsTesoro (o:os) = esTesoro o || algunoEsTesoro os

esTesoro :: Objeto -> Bool 
esTesoro Tesoro = True 
esTesoro _      = False

-- 2 

hayTesoroEn :: [Dir] -> Mapa -> Bool
-- En caso de que haya mas direcciones que camino por recorrer retorna False
hayTesoroEn [] m                          = tieneTesoro (cofreDe m)
hayTesoroEn _ (Fin _)                     = False
hayTesoroEn (d:ds) m = hayTesoroEn ds (irHacia m d)

cofreDe :: Mapa -> Cofre  
cofreDe (Bifurcacion c _ _ ) = c
cofreDe (Fin c)              = c

irHacia :: Mapa -> Dir -> Mapa
-- Precond: El mapa es bifurcacion 
irHacia (Bifurcacion _ _ md) Der = md 
irHacia (Bifurcacion _ mi _) Izq = mi


-- 3

caminoAlTesoro :: Mapa -> [Dir]
-- Precond: existe un tesoro y es unico
caminoAlTesoro (Fin _)               = []
caminoAlTesoro (Bifurcacion _ mi md) = singularSi Izq (hayTesoro mi) 
                                    ++ singularSi Der (hayTesoro md)
                                    ++ caminoAlTesoro mi 
                                    ++ caminoAlTesoro md

-- 4

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ mi md) = if (esMarLargoQue mi md) 
                                               then Izq : caminoDeLaRamaMasLarga (mi)
                                               else Der : caminoDeLaRamaMasLarga (md)

esMarLargoQue :: Mapa -> Mapa -> Bool 
esMarLargoQue m1 m2 = longitudDe m1 > longitudDe m2

longitudDe :: Mapa -> Int 
longitudDe (Fin _)               = 0 
longitudDe (Bifurcacion _ mi md) = 1 + max (longitudDe mi) (longitudDe md)

-- 5 


