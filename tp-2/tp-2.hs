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
mayoresA n (p:ps) = if  (edad p) > n 
                      then p : mayoresA n ps 
                    else mayoresA n ps 

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

unoSiCeroSino :: Bool -> Int 
unoSiCeroSino True  = 1
unoSiCeroSino False = 0 

pokemonEsDeTipo :: Pokemon -> TipoDePokemon -> Bool 
pokemonEsDeTipo (ConsPokemon Fuego  _ ) Fuego  = True 
pokemonEsDeTipo (ConsPokemon Agua   _ ) Agua   = True 
pokemonEsDeTipo (ConsPokemon Planta _ ) Planta = True 
pokemonEsDeTipo        _           _           = False

cantPokemonsDeTipoEn :: TipoDePokemon -> [Pokemon] -> Int 
cantPokemonsDeTipoEn _ []     = 0
cantPokemonsDeTipoEn t (p:ps) = (unoSiCeroSino (pokemonEsDeTipo p t)) + (cantPokemonsDeTipoEn t ps)

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantPokemonDe t e = cantPokemonsDeTipoEn t (listaPokemonDe e)

-- c 

--cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int


-- d

hayDeTipo_En_ :: TipoDePokemon -> [Pokemon] -> Bool 
hayDeTipo_En_  _ []     = False
hayDeTipo_En_  t (p:ps) = (pokemonEsDeTipo p t) || (hayDeTipo_En_ t ps)

esMaestroPokemon :: Entrenador -> Bool 
esMaestroPokemon (ConsEntrenador _ ps ) = (hayDeTipo_En_ Fuego ps) && (hayDeTipo_En_ Agua ps) && (hayDeTipo_En_ Planta ps)