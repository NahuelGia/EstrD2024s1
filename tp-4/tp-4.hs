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


-- 3 Pendiente

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

idCarga :: SectorId
idCarga = "SalaDeCarga"

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
           (NodeT (S "SalaDeMaquinas" [Motor 3] ["Ingeniero", "Tripulante1"]) EmptyT EmptyT)
           (NodeT (S "SalaDeCarga" [Almacen barrilesAlmacen] ["Ingeniero", "Carlos"]) EmptyT EmptyT)

-- Ejemplo de una nave que contiene un árbol de sectores
naveEjemplo :: Nave
naveEjemplo = N arbolSectores

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show    
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show

type SectorId = String

type Tripulante = String
   
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
   deriving Show
data Nave = N (Tree Sector)
    deriving Show

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

-- 4 

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N t) = (N (agregarASectorEnTree cs id t) )

agregarASectorEnTree :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorEnTree _  _  EmptyT          = EmptyT
agregarASectorEnTree cs id (NodeT s t1 t2) = if sectorTieneId s id
                                             then (NodeT (agregarComponentes s cs) t1 t2)
                                             else (NodeT s (agregarASectorEnTree cs id t1)
                                                           (agregarASectorEnTree cs id t2))

sectorTieneId :: Sector -> SectorId -> Bool 
sectorTieneId (S id1 _ _ ) id2 = id1 == id2

agregarComponentes :: Sector -> [Componente] -> Sector 
agregarComponentes (S id cs1 ts) cs2 = (S id (cs1++cs2) ts)

-- 5  

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave 
-- Precond: Todos los id de la lista existen en la nave 
asignarTripulanteA trip ids (N tree) = (N (asignarTripulanteAEnTree trip ids tree))

asignarTripulanteAEnTree :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector 
asignarTripulanteAEnTree trip ids EmptyT          = EmptyT
asignarTripulanteAEnTree trip ids (NodeT s t1 t2) = if sectorSeEncuentraEnLista s ids 
                                                    then (NodeT (asignarTriupalenteASector s trip) 
                                                                (asignarTripulanteAEnTree trip ids t1)
                                                                (asignarTripulanteAEnTree trip ids t2))
                                                    else (NodeT s
                                                                (asignarTripulanteAEnTree trip ids t1)
                                                                (asignarTripulanteAEnTree trip ids t2) )

asignarTriupalenteASector :: Sector -> Tripulante -> Sector 
asignarTriupalenteASector (S id cs ts1) ts2 = (S id cs (ts2 : ts1 ))

sectorSeEncuentraEnLista :: Sector -> [SectorId] -> Bool 
sectorSeEncuentraEnLista s []       = False
sectorSeEncuentraEnLista s (id:ids) = (sectorTieneId s id) || (sectorSeEncuentraEnLista s ids )

-- 6

sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados  trip (N tree) = sectoresAsignadosEnTree trip tree 

sectoresAsignadosEnTree :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosEnTree _    EmptyT          = []
sectoresAsignadosEnTree trip (NodeT s t1 t2) = singularSi (idSector s) (seEncuentraAsignado trip s)
                                             ++ sectoresAsignadosEnTree trip t1 
                                             ++ sectoresAsignadosEnTree trip t2

seEncuentraAsignado :: Tripulante -> Sector -> Bool  
seEncuentraAsignado t (S _ _ ts ) = pertenece t ts 
    
pertenece :: Eq a => a -> [a] -> Bool 
pertenece e []     = False  
pertenece e (x:xs) = (e == x) || pertenece e xs

-- 7 

tripulantes :: Nave -> [Tripulante]
tripulantes (N t) = tripulantesEnTree t 

tripulantesEnTree :: Tree Sector -> [Tripulante]
tripulantesEnTree EmptyT          = []
tripulantesEnTree (NodeT s t1 t2) = juntarSinRepetir (tripulantesDe s) (
                                    juntarSinRepetir (tripulantesEnTree t1) 
                                                     (tripulantesEnTree t2))

tripulantesDe :: Sector -> [Tripulante]
tripulantesDe (S _ _ ts) = ts 

juntarSinRepetir :: Eq a => [a] -> [a] -> [a]
juntarSinRepetir [] ys     = ys
juntarSinRepetir (x:xs) ys = singularSi x (not (pertenece x ys)) ++ juntarSinRepetir xs ys 

{- EJERCICIO 4 -}

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
          | Explorador Nombre [Territorio] Lobo Lobo
          | Cria Nombre
data Manada = M Lobo

-- 1

manada1 :: Manada 
manada1 = (M (Cazador "Cazador" ["presa"] 
             (Explorador "Explorador1" ["territorio1"] (Cria "cria2") (Cria "cria3") ) 
             (Explorador "Explorador2" ["territorio2"] (Cria "cria4") (Cria "cria5") )
             (Cria "cria1")))

-- 2 

buenaCaza :: Manada -> Bool
buenaCaza (M l) = nroTotalPresas l > nroHijos l 

nroHijos :: Lobo -> Int 
nroHijos (Cria _)               = 1 
nroHijos (Explorador _ _ l1 l2) = (nroHijos l1) + (nroHijos l2)
nroHijos (Cazador _ _ l1 l2 l3) = (nroHijos l1) + (nroHijos l2) + (nroHijos l3)

nroTotalPresas :: Lobo -> Int 
nroTotalPresas (Cria _)                = 0 
nroTotalPresas (Explorador _ _ l1 l2)  = (nroTotalPresas l1) + (nroTotalPresas l2)
nroTotalPresas (Cazador _ pr l1 l2 l3) = (length pr) + (nroTotalPresas l1) + (nroTotalPresas l2) + (nroTotalPresas l3)

-- 3 

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaEn l 

elAlfaEn :: Lobo -> (Nombre, Int)
elAlfaEn (Cria n)                 = (n,0)
elAlfaEn (Explorador _ _ l1 l2)   = elZipConMasPresasEntre (elAlfaEn l1) (elAlfaEn l2)         
elAlfaEn (Cazador n pr l1 l2 l3 ) = elZipConMasPresasEntre (n, length pr)( 
                                    elZipConMasPresasEntre (elAlfaEn l1)(
                                    elZipConMasPresasEntre (elAlfaEn l2)  (elAlfaEn l3)) )    

elZipConMasPresasEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elZipConMasPresasEntre z1 z2 = if (snd z1 > snd z2) 
                                  then z1 
                                  else z2 

-- 4 

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronEn t l 

losQueExploraronEn :: Territorio -> Lobo -> [Nombre]
losQueExploraronEn t (Cria n)                = []
losQueExploraronEn t (Explorador n ts l1 l2) = singularSi n (pertenece t ts)
                                            ++ losQueExploraronEn t l1 
                                            ++ losQueExploraronEn t l2
losQueExploraronEn t (Cazador _ _ l1 l2 l3 ) = losQueExploraronEn t l1 
                                            ++ losQueExploraronEn t l2 
                                            ++ losQueExploraronEn t l3

-- 5

exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioEn l 

exploradoresPorTerritorioEn :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioEn (Cria n)                = []
exploradoresPorTerritorioEn (Explorador n ts l1 l2) = sumarTerritoriosDeALista n ts 
                                                      ( juntarListaTuplasPorTerritorio 
                                                        (exploradoresPorTerritorioEn l1)
                                                        (exploradoresPorTerritorioEn l2) )
exploradoresPorTerritorioEn (Cazador _ _ l1 l2 l3 ) = juntarListaTuplasPorTerritorio (exploradoresPorTerritorioEn l1) (
                                                      juntarListaTuplasPorTerritorio (exploradoresPorTerritorioEn l2)
                                                                                     (exploradoresPorTerritorioEn l3) )

juntarListaTuplasPorTerritorio :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
juntarListaTuplasPorTerritorio []     ys = ys 
juntarListaTuplasPorTerritorio (x:xs) ys = sumarTupla x ys ++ juntarListaTuplasPorTerritorio xs ys

sumarTupla :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
sumarTupla x []     = [(fst x , snd x)]
sumarTupla x (y:ys) = if (fst x) == (fst y)
                   -- fst = Territorio | snd = nombres
                      then (fst x, (snd x) ++ (snd y)) : ys
                      else y : sumarTupla x ys 
            

sumarTerritoriosDeALista :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
sumarTerritoriosDeALista n []     zs = zs  
sumarTerritoriosDeALista n (t:ts) zs = sumarNombreYTerritorio n t zs  
                                    ++ sumarTerritoriosDeALista n ts zs 

sumarNombreYTerritorio :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
sumarNombreYTerritorio n t []     = [(t,[n])]
sumarNombreYTerritorio n t (z:zs) = if t == (fst z)
                                    then (fst z, n : snd z ) : zs
                                    else z : sumarNombreYTerritorio n t zs


-- 6 Pendiente

superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = superioresDelCazadorEn n l

superioresDelCazadorEn :: Nombre -> Lobo -> Maybe [Nombre]
superioresDelCazadorEn n (Cria _)                 = Nothing 
superioresDelCazadorEn n (Explorador _ _ l1 l2)   = appenMaybe (superioresDelCazadorEn n l1)  (superioresDelCazadorEn n l2)
superioresDelCazadorEn n (Cazador n2 _ l1 l2 l3 ) = if (n == n2)
                                                    then Just []
                                                    else Just (n2 ++ ( appenMaybe                                                    
                                                                               (superioresDelCazadorEn n l1)
                                                                               (appenMaybe (superioresDelCazadorEn n l2)
                                                                                           (superioresDelCazadorEn n l3 ))))
                    
appenMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
appenMaybe Nothing Nothing     = Nothing
appenMaybe Nothing (Just ys)   = Just ys 
appenMaybe (Just xs) Nothing   = Just xs 
appenMaybe (Just xs) (Just ys) = Just (xs ++ ys)