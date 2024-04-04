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

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [tesorosEn c]
tesorosPorNivel (Bifurcacion c mi md) = tesorosEn c : agruparPorNivel (tesorosPorNivel mi) (tesorosPorNivel md)

agruparPorNivel :: [[a]] -> [[a]] -> [[a]]
agruparPorNivel [] []         = []
agruparPorNivel xs []         = xs
agruparPorNivel [] ys         = ys  
agruparPorNivel (x:xs) (y:ys) = (x ++ y) : (agruparPorNivel xs ys)

tesorosEn :: Cofre -> [Objeto]
tesorosEn (Cofre [])     = []
tesorosEn (Cofre (o:os)) = singularSi o (esTesoro o)  ++ tesorosEn (Cofre os)

-- 6 

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = [[]]
todosLosCaminos (Bifurcacion _ mi md) = consACada Izq (todosLosCaminos mi) 
                                     ++ consACada Der (todosLosCaminos md)

consACada :: a -> [[a]] -> [[a]]
consACada e []       = []
consACada e (xs:xss) = (e:xs) : consACada e xss 

{- EJERCICIO 3 -}

-- Ejemplo de componentes
componentesEjemplo :: [Componente]
componentesEjemplo = [LanzaTorpedos, Motor 5, Almacen [Comida, Oxigeno, Torpedo]]

-- Ejemplo de barriles en un almacén
barrilesAlmacen :: [Barril]
barrilesAlmacen = [Comida, Comida, Combustible, Oxigeno]

-- Ejemplo de un sector con componentes y tripulantes
sectorEjemplo :: Sector
sectorEjemplo = S "SalaDeControl" componentesEjemplo ["Tripulante1", "Tripulante2"]

-- Ejemplo de un árbol de sectores
arbolSectores :: Tree Sector
arbolSectores =
    NodeT sectorEjemplo
           (NodeT (S "SalaDeMaquinas" [Motor 3] ["Ingeniero"]) EmptyT EmptyT)
           (NodeT (S "SalaDeCarga" [Almacen barrilesAlmacen] []) EmptyT EmptyT)

-- Ejemplo de una nave que contiene un árbol de sectores
naveEjemplo :: Nave
naveEjemplo = N arbolSectores

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
        
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    
type SectorId = String
    
type Tripulante = String
   
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
   
data Nave = N (Tree Sector)
   

-- 1 

sectores :: Nave -> [SectorId]
sectores (N t)      = idSectoresEnTree t  

idSectoresEnTree :: Tree Sector -> [SectorId]
idSectoresEnTree EmptyT          = []
idSectoresEnTree (NodeT s t1 t2) = idSector s : idSectoresEnTree t1 ++ idSectoresEnTree t2

idSector :: Sector -> SectorId
idSector (S id _ _ ) = id

-- 2 

poderDePropulsion :: Nave -> Int 
poderDePropulsion (N t) = poderDePropulsionEnTree t 

poderDePropulsionEnTree :: Tree Sector -> Int 
poderDePropulsionEnTree EmptyT          = 0
poderDePropulsionEnTree (NodeT s t1 t2) = poderDePropulsionDeComponentes (componentes s) 
                                        + poderDePropulsionEnTree t1 
                                        + poderDePropulsionEnTree t2 

componentes :: Sector -> [Componente]
componentes (S _ cs _) = cs

poderDePropulsionDeComponentes :: [Componente] -> Int 
poderDePropulsionDeComponentes []     = 0
poderDePropulsionDeComponentes (c:cs) = poderDePropulsionSiEsMotor c 
                                      + poderDePropulsionDeComponentes cs

poderDePropulsionSiEsMotor :: Componente -> Int 
poderDePropulsionSiEsMotor (Motor _ ) = 1 
poderDePropulsionSiEsMotor _          = 0 


-- 3 

barriles :: Nave -> [Barril]
barriles (N t) = barrilesEnTree t 

barrilesEnTree :: Tree Sector -> [Barril]
barrilesEnTree EmptyT          = []
barrilesEnTree (NodeT s t1 t2) = barrilesEnLista (componentes s) 
                              ++ barrilesEnTree t1 
                              ++ barrilesEnTree t2 

barrilesEnLista :: [Componente] -> [Barril]
barrilesEnLista []     = []
barrilesEnLista (c:cs) = barrilesSiEsAlmacen c ++ barrilesEnLista cs 

barrilesSiEsAlmacen :: Componente -> [Barril] 
barrilesSiEsAlmacen (Almacen bs) = bs 
barrilesSiEsAlmacen _            = []
