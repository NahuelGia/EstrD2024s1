{- EJERCICIO 2 -}

-- 1:

-- a

sucesor :: Int -> Int
sucesor n = n+1

-- b 

sumar :: Int -> Int -> Int 
sumar n m = n + m

-- c 

divisionYResto :: Int -> Int -> (Int,Int)
divisionYResto n m = (div n m, mod n m)

-- d 

maxDelPar :: (Int, Int) -> Int
maxDelPar (n,m) = max n m

-- 2:

-- 1. sucesor(maxDelPar( divisionYResto (sumar 20 7) 3 ))

-- 2. maxDelPar(divisionYResto (sucesor 9) 1)

-- 3. sucesor( sumar (maxDelPar(divisionYResto 4 11 )) 5 )

-- 4. sumar (maxDelPar(divisionYResto 8 10) ) 2 

{- EJERCICIO 3 -}

-- 1:

data Dir = Norte | Este | Sur | Oeste
    deriving Show 

-- a

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este  = Oeste
opuesto Sur   = Norte 
opuesto Oeste = Este
        
-- b 

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este   = True
iguales Oeste Oeste = True 
iguales Sur Sur     = True 
iguales _ _         = False 

-- c 

-- En caso de que no exista la siguiente dirección a Oeste. La función tendria como precondición
-- que la dirección no puede ser Oeste y seria parcial.

siguiente :: Dir -> Dir 
siguiente Norte = Este 
siguiente Este  = Sur 
siguiente Sur   = Oeste 
siguiente Oeste = Norte 

-- 2: 

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 
    deriving Show

-- a

primerDia :: DiaDeSemana
primerDia = Lunes 

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

-- b 

empiezaConM :: DiaDeSemana -> Bool 
empiezaConM Martes    = True 
empiezaConM Miercoles = True 
empiezaConM _         = False 

-- c 

numeroDia :: DiaDeSemana -> Int

numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5
numeroDia Sabado    = 6
numeroDia d         = 7

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = (numeroDia d1) > (numeroDia d2 )


-- d

estaEnElMedio :: DiaDeSemana -> Bool 
estaEnElMedio Lunes   = False 
estaEnElMedio Domingo = False 
estaEnElMedio _       = True 

-- 3:

-- a 

negar :: Bool -> Bool 
negar True  = False 
negar False = True 

-- b

implica :: Bool -> Bool -> Bool 
implica True False = False
implica _ _        = True 

-- c

yTambien :: Bool -> Bool -> Bool 
yTambien _     False = False 
yTambien False _ = False
yTambien _     _  = True

-- d 

oBien :: Bool -> Bool -> Bool
oBien True _ = True 
oBien _    b = b

{- EJERCICIO 4 -}

-- 1 :

data Persona = P String Int 
              -- Nombre Edad
    deriving Show

hector = P "Hector" 20
pedro  = P "Pedro"  42

-- a

nombre :: Persona -> String 
nombre (P n _) = n

-- b 

edad :: Persona -> Int 
edad (P _ e) = e 

-- c 

crecer :: Persona -> Persona 
crecer (P n e) = P n (e+1) 

-- d

cambioDeNombre :: String -> Persona -> Persona 
cambioDeNombre n (P _ e) = P n e 

-- e

esMayorQueLaOtra :: Persona -> Persona -> Bool 
esMayorQueLaOtra p1 p2 = (edad p1) > (edad p2)

-- f 

laQueEsMayor :: Persona -> Persona -> Persona 
laQueEsMayor p1 p2 = if (esMayorQueLaOtra p1 p2)
                        then p1 
                     else p2

-- 2:

data TipoDePokemon = Agua | Fuego | Planta 
    deriving Show

data Pokemon = Poke TipoDePokemon Int 
                               -- energia 
    deriving Show 

data Entrenador = E String Pokemon Pokemon 
                 -- nombre
    deriving Show 

charizard  = Poke Fuego 60 
mudkip     = Poke Agua  100
entrenador = E "prueba" charizard mudkip

-- a

tipo :: Pokemon -> TipoDePokemon
tipo (Poke t _) = t 

tipoSuperaA :: TipoDePokemon -> TipoDePokemon -> Bool 
tipoSuperaA Agua   Fuego  = True 
tipoSuperaA Fuego  Planta = True 
tipoSuperaA Planta Agua   = True 
tipoSuperaA _ _           = False 

superaA :: Pokemon -> Pokemon -> Bool 
superaA p1 p2 = tipoSuperaA (tipo p1) (tipo p2)

-- b 

unoSiCeroSino :: Bool -> Int 
unoSiCeroSino True  = 1
unoSiCeroSino False = 0 

pokemonEsDeTipo :: Pokemon -> TipoDePokemon -> Bool 
pokemonEsDeTipo (Poke Fuego  _ ) Fuego  = True 
pokemonEsDeTipo (Poke Agua   _ ) Agua   = True 
pokemonEsDeTipo (Poke Planta _ ) Planta = True 
pokemonEsDeTipo        _           _    = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantidadDePokemonDe t (E _ p1 p2 ) = (unoSiCeroSino(pokemonEsDeTipo p1 t )) + (unoSiCeroSino(pokemonEsDeTipo p2 t ))

-- c 

listaPokemonDe :: Entrenador -> [Pokemon]
listaPokemonDe (E _ p1 p2) = [p1,p2]

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = (listaPokemonDe e1) ++ (listaPokemonDe e2)

{- EJERCICIO 5 -}

-- 1:

-- a

loMismo :: a -> a 
loMismo a = a

-- b 

siempreSiete :: a -> Int 
siempreSiete a = 7

-- c 

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- 2: Son polimórficas porque pueden funcionar con cualquier tipo de dato y retornar algo 

{- EJERCICIO 6 -}

-- 2 

estaVacia :: [a] -> Bool 
estaVacia [] = True 
estaVacia _  = False  

-- 3 

elPrimero :: [a] -> a 
elPrimero (x:_) = x
elPrimero _ = error "La lista esta vacia"

-- 4 

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : xs) = xs 
sinElPrimero _ = error "La lista esta vacia"

-- 5

splitHead :: [a] -> (a, [a])
splitHead (x : xs) = (x, xs)
splitHead _ = error "La lista esta vacia"
