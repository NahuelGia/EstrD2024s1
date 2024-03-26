
singularSi :: a -> Bool -> [a]
singularSi e True  = [e]
singularSi _ False = []

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
pertence e (x:xs) = (e == x) || pertence e xs

-- 8 

apariciones :: Eq a => a -> [a] -> Int 
apariciones _ []     =  0 
apariciones e (x:xs) = unoSi (e == x) + apariciones e xs 
-- 9 

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []     = []
losMenoresA n (x:xs) = singularSi x (x<n) ++ losMenoresA n xs 

-- 10 

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []     = []
lasDeLongitudMayorA n (x:xs) = singularSi x ((longitud x) > n) ++ lasDeLongitudMayorA n xs 

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
zipMaximos (x:xs) (y:ys) = (max x y) : zipMaximos xs ys  

-- 15

elMinimo :: Ord a => [a] -> a 
-- Precond: La lista no puede ser vacia 
elMinimo [x]    = x 
elMinimo (x:xs) = min x (elMinimo xs)

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

-- 5 

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs     = xs
sinLosPrimeros n []     = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 

{- EJERCICIO 3 -}

-- 1 

data Persona = P String Int 
  deriving Show

edad :: Persona -> Int 
edad (P _ e) = e

hector = P "Hector" 20
pedro  = P "Pedro"  42
pepe   = P "Pepe"   38
listaPersonas = [hector,pedro,pepe]

-- a

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []     = []
mayoresA n (p:ps) = singularSi p ((edad p) > n ) ++ mayoresA n ps

-- b

sumatoriaDeEdades :: [Persona] -> Int 
sumatoriaDeEdades []     = 0
sumatoriaDeEdades (x:xs) = (edad x) + (sumatoriaDeEdades xs) 

promedioEdad :: [Persona] -> Int 
promedioEdad ps  = div (sumatoriaDeEdades ps) (longitud ps)  

-- c

elMasViejo :: [Persona] -> Persona
elMasViejo [p]    = p 
elMasViejo (p:ps) = if (edad p) > (edad (elMasViejo ps))
                      then p 
                    else elMasViejo ps 

-- 2 

data TipoDePokemon = Agua | Fuego | Planta
  deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int
  deriving Show 

data Entrenador = ConsEntrenador String [Pokemon]
  deriving Show 

charizard  = ConsPokemon Fuego  60 
mudkip     = ConsPokemon Agua   100
juan       = ConsPokemon Planta 50
entrenador = ConsEntrenador "prueba" [charizard, mudkip, juan]

-- a

listaPokemonDe :: Entrenador -> [Pokemon]
listaPokemonDe (ConsEntrenador _ ps) = ps 

cantPokemon :: Entrenador -> Int 
cantPokemon e = longitud (listaPokemonDe e)

-- b 

unoSi :: Bool -> Int 
unoSi True  = 1
unoSi False = 0 

pokemonEsDeTipo :: Pokemon -> TipoDePokemon -> Bool 
pokemonEsDeTipo p t = elTipoEsIgualA (tipoDe p) t 

elTipoEsIgualA :: TipoDePokemon -> TipoDePokemon -> Bool 
elTipoEsIgualA Fuego  Fuego  = True 
elTipoEsIgualA Agua   Agua   = True 
elTipoEsIgualA Planta Planta = True 
elTipoEsIgualA _      _      = False

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t _) = t 

cantPokemonsDeTipoEn :: TipoDePokemon -> [Pokemon] -> Int 
cantPokemonsDeTipoEn _ []     = 0
cantPokemonsDeTipoEn t (p:ps) = (unoSi (pokemonEsDeTipo p t)) + (cantPokemonsDeTipoEn t ps)

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantPokemonDe t e = cantPokemonsDeTipoEn t (listaPokemonDe e)

-- c 

cuantosDeTipoDe_LeGananATodosLosDe :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipoDe_LeGananATodosLosDe t (ConsEntrenador _ pks1) (ConsEntrenador _ pks2) = cantPokesDeTipoQueLeGananATodos t pks1 pks2


cantPokesDeTipoQueLeGananATodos :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int 
cantPokesDeTipoQueLeGananATodos _  []           pks2 = 0
cantPokesDeTipoQueLeGananATodos t  (pk1 : pks1) pks2 = if  pokemonEsDeTipo pk1 t

                                                          then unoSi (venceATodos pk1 pks2)
                                                                + cantPokesDeTipoQueLeGananATodos t pks1 pks2

                                                        else cantPokesDeTipoQueLeGananATodos t pks1 pks2

tipo :: Pokemon -> TipoDePokemon 
tipo (ConsPokemon t _ ) = t


venceATodos :: Pokemon -> [Pokemon] -> Bool
venceATodos p1   []          = True
venceATodos pk1  (pk2: pks2) = (superaA pk1 pk2) && venceATodos pk1 pks2

tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool 
tipoSuperaA Agua   Fuego  = True 
tipoSuperaA Fuego  Planta = True 
tipoSuperaA Planta Agua   = True 
tipoSuperaA _ _           = False 

superaA :: Pokemon -> Pokemon -> Bool 
superaA p1 p2 = tipoSuperaA (tipo p1) (tipo p2)


-- d 

hayDeTipo_En_ :: TipoDePokemon -> [Pokemon] -> Bool 
hayDeTipo_En_  _ []     = False
hayDeTipo_En_  t (p:ps) = (pokemonEsDeTipo p t) || (hayDeTipo_En_ t ps)

esMaestroPokemon :: Entrenador -> Bool 
esMaestroPokemon (ConsEntrenador _ ps ) = (hayDeTipo_En_ Fuego ps) && (hayDeTipo_En_ Agua ps) && (hayDeTipo_En_ Planta ps)

-- 3

data Seniority = Junior | SemiSenior | Senior
  deriving Show

data Proyecto  = ConsProyecto String
  deriving (Show, Eq)

data Rol       = Developer Seniority Proyecto | Management Seniority Proyecto
  deriving Show

data Empresa   = ConsEmpresa [Rol]
  deriving Show

proyecto1 = ConsProyecto "proyecto1"
proyecto2 = ConsProyecto "proyecto2"
proyecto3 = ConsProyecto "proyecto3"
proyecto4 = ConsProyecto "proyecto4"

rol1 = Developer Junior proyecto1
rol2 = Developer Senior proyecto2
rol3 = Management SemiSenior proyecto4
rol4 = Management SemiSenior proyecto4

empresa1 = ConsEmpresa [rol1, rol2, rol3, rol4]

-- a

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosEnRoles rs

proyectosEnRoles :: [Rol] -> [Proyecto]
proyectosEnRoles []     = []
proyectosEnRoles (r:rs) = if pertence (proyectoDelRol r) (proyectosEnRoles rs)
                            then proyectosEnRoles rs
                          else (proyectoDelRol r) : proyectosEnRoles rs

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer  _ p) = p
proyectoDelRol (Management _ p) = p

-- b

losDevSenior :: Empresa -> [Proyecto] -> Int 
losDevSenior (ConsEmpresa rs) ps = cantDevSeniorConProyecto rs ps

esDevSeniorConProyecto :: Rol -> [Proyecto] -> Bool 
esDevSeniorConProyecto (Developer Senior p ) ps = (pertence p ps)
esDevSeniorConProyecto _                     _  = False

cantDevSeniorConProyecto :: [Rol] -> [Proyecto] -> Int 
cantDevSeniorConProyecto []     _  = 0
cantDevSeniorConProyecto (r:rs) ps = (unoSi (esDevSeniorConProyecto r ps)) + (cantDevSeniorConProyecto rs ps)

-- c

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int 
cantQueTrabajanEn pr (ConsEmpresa rs) = cantRolesConProyecto rs pr 

cantRolesConProyecto :: [Rol] -> [Proyecto] -> Int 
cantRolesConProyecto []     _  = 0
cantRolesConProyecto (r:rs) ps = unoSi (pertence (proyectoDelRol r) ps) + cantRolesConProyecto rs ps 

-- d 

asignadosPorProyecto :: Empresa -> [(Proyecto,Int)]
asignadosPorProyecto e = cantRolesPorProyecto (listaRoles e) (proyectos e)

listaRoles :: Empresa -> [Rol]
listaRoles (ConsEmpresa rs) = rs 

cantRolesPorProyecto :: [Rol]  -> [Proyecto] -> [(Proyecto, Int)]
cantRolesPorProyecto rs []     = []
cantRolesPorProyecto rs (p:ps) = (p, rolesAsignadosEn rs p ) : cantRolesPorProyecto rs ps

rolesAsignadosEn :: [Rol] -> Proyecto -> Int 
rolesAsignadosEn []     p = 0
rolesAsignadosEn (r:rs) p = unoSi ( (proyectoDelRol r) == p ) + rolesAsignadosEn rs p