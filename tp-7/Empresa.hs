module Empresa 
    (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, 
     agregarSector, agregarEmpleado, agregarASector, borrarEmpleado )
where 

-- import Set 
-- import Map 
-- import Empleado 

type SectorId = Int
type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)
{-
INV.REP: En ConsE map1 map2 
* Si el map2 esta vacío, ningún sector del map1 tiene empleados 
* Un empleado tiene solo un CUIL 
* Todos los empleados del set de map1 corresponden a un empleado del map2. Es decir que no hay un empleado de map1 que no pertenezca a map2. 
* Los sectores de los empleados de map1 y de map2 corresponden a ids de sectores del map1
! PREGUNTAR
? Como funcionan las instancias en funcional? Ejemplo: Si tengo un empleado en varios Sets por cada proyecto y yo le borro
? al empleado el proyecto, este se borra en los otros Sets también? 
? Al implementar un dato abstracto en el parcial, tengo que escribir toda la estructura como se haría en 
? una computadora? (Poner module, las funciones where, etc)
-}


consEmpresa :: Empresa
-- Propósito: construye una empresa vacía.
-- Costo: O(1)
buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- Propósito: devuelve el empleado con dicho CUIL.
-- Precondición: el CUIL es de un empleado de la empresa.
-- Costo: O(log E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(log S + E)
todosLosCUIL :: Empresa -> [CUIL]
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosSectores :: Empresa -> [SectorId]
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
agregarSector :: SectorId -> Empresa -> Empresa
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S)
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el
-- CUIL dado.
-- Costo: calcular.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: calcular.
borrarEmpleado :: CUIL -> Empresa -> Empresa
-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: calcular

consEmpresa = ConsE emptyM emptyM

-- Es O(log E) debido al costo de lookupM que en este caso está relacionado a la cantidad 
-- de empleados que tiene el Map . 
buscarPorCUIL cuil (ConsE _ map2 ) = case lookupM cuil map2 of 
                                     Nothing -> error "El CUIL no corresponde a un empleado"
                                     Just e  -> e 

-- Es O(log S + E), en este caso la S corresponde al costo lookupM el cual se relaciona
-- al nro de Sectores que tiene el map y la E es ya que el setToList es de costo lineal
-- sobre la cantidad de Empleados que tiene el Set 
empleadosDelSector idSector (ConsE map1 _ ) = case lookupM idSector map1 of 
                                              Nothing -> []
                                              Just s  -> setToList s 

-- Es O(log E) debido al costo de keys que depende del nro de empleados que tenga el map 
todosLosCUIL (ConsE _ map2) = keys map2

todosLosSectores (ConsE map1 _) = keys map1 

-- Es O(log S) por el costo de assocM que depende del nro de Sectores del map 
-- ? Ver que pasa si quiero agregar un sector que ya existe ya que si pierdo a los empleados
-- ? a estos no se les modificaria el id de sectores pero ya no estarían trabajando allí 
agregarSector sectorId (ConsE map1 map2) = ConsE (assocM sectorId emptyS map1) map2 
                                                -- k      v   
                                        -- *case lookupM sectorId map1 of O(log S + log S)
                                        -- *Nothing -> ConsE (assocM sectorId emptyS map1) map2 
                                        -- *Just x  -> ConsE map1 map2

-- O((S * log N) + (S * (2 log M + log E)) )
-- ? Que pasa si quiero agregar a un empleado que ya existe y además le paso sectores distintos 
-- ? a los que tiene asignados. Y tambien que pasa si le paso un sector que no existe .
agregarEmpleado ids cuil (ConsE map1 map2) = let 
                                             empleado = incorporarSectoresAEmpleado ids (consEmpleado cuil)
                                             in 
                                             ConsE (agregarEmpleadoASectores ids empleado map1 ) --map1
                                                   (assocM cuil empleado map2) -- map2

                                            -- *case lookupM cuil map2 of 
                                            -- *Nothing -> ConsE (agregarEmpleadoASectores ids empleado map1 ) --map1
                                            -- *                 (assocM cuil empleado map2)
                                            -- *Just _  -> ConsE map1 map2


-- O(N * (log S + log S ) ) = (S * (2 log S + log E))
-- Donde N es la cantidad de ids de la lista sobre la que se hace RE,
-- S es la cantidad de sectores del Map y E es la cantidad de empleados del Set
-- Justificación: Por cada id de la lista se realizan las siguientes funciones:
-- lookupM O(log S)
-- assocM  O(log S)
-- addS    O(log E)
agregarEmpleadoASectores :: [SectorId] -> Empleado -> Map SectorId (Set empleado) -- ? Que pasa hay un sector que no existe?
agregarEmpleadoASectores []       e map = map 
agregarEmpleadoASectores (id:ids) e map = case lookupM id map of
                                          Just set -> assocM id (addS e set) (agregarEmpleadoASectores ids e map)
                                          Nothing  -> agregarEmpleadoASectores ids e map  -- *error "La id no corresponde a un sector existente"
                                                    -- ! Si hago esto está mal porque ya incorporé los sectores al Empleado

-- O(S * log N) 
-- Donde S es el nro de ids de la lista sobre la que se hace RE y N es la cantidad de Sectores que 
-- tiene el empleado .
-- El costo es este porque por cada id se llama a la funcion incorporarSector de costo log N 
-- en este caso es N y no S porque el nro de sectores del empleado puede ser diferente al de la lista
incorporarSectoresAEmpleado :: [SectorId] -> Empleado -> Empleado 
incorporarSectoresAEmpleado []       e = e 
incorporarSectoresAEmpleado (id:ids) e = incorporarSector id (incorporarSectoresAEmpleado ids e)


-- O (log E # Por lookupM sobre el nro de empleados en map2
--    + (3 log S + 2 log E ) # por costo de agregarASectorSiExiste ) -- ! Preguntar
agregarASector id cuil (ConsE map1 map2) = case lookupM cuil map2 of  -- Chequeo que el empleado exista    
                                           Nothing -> ConsE map1 map2 -- Si no existe no hago nada 
                                           Just e  -> agregarASectorSiExiste (ConsE map1 map2) -- Lo agrego al sector solo si existe

-- O( log S # Por lookupM sobre sobre el nro de Sectores en map1
--  + log S # Por assocM sobre el nro de sectores en map1
--  + log E # Por addS sobre el nro de empleados del Set en map1 
--  + log E # Por assocM sobre el nro de empleados en map2 
--  + log S # Por incorporarSector sobre el nro de sectores del empleado que por Inv en el peor caso 
           -- es igual al nro de sectores del map1).
agregarASectorSiExiste :: SectorId -> Empleado -> Map Sector (Set Empleado) -> Map CUIL Empleado -> Empresa 
agregarASectorSiExiste id e map1 map2 = case lookupM id map1 of  -- Chequeo que el sector exista
                                             Nothing -> ConsE map1 map2  -- Si no existe no hago nada 
                                             Just s  -> ConsE (assocM id (addS e s) map1)
                                                              (assocM cuil (incorporarSector id e) map2)
-- ! Calcular costo 
borrarEmpleado cuil (ConsE map1 map2) = case lookupM cuil map2 of 
                                        Nothing -> ConsE map1 map2 
                                        Just e  -> ConsE (sacarDeSectoresA (sectores e) e map1)
                                                         (deleteM cuil map2)

sacarDeSectoresA :: [SectorId] -> Empleado -> Map SectorId (Set empleado)
sacarDeSectoresA []       e map = map 
sacarDeSectoresA (id:ids) e map = case lookupM id map of 
                                  Nothing -> map 
                                  Just s  -> assocM id (removeS e s) (sacarDeSectoresA ids e map)
