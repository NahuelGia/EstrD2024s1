module Empleado
    (Empleado,consEmpleado,cuil,incorporarSector,sectores)
    where

import Set

type CUIL       = Int
type SectorId   = Int

data Empleado = E CUIL (Set SectorId)
    

-- Propósito : construye un empleado con dicho CUIL.
-- Costo : O(1)
consEmpleado :: CUIL    -> Empleado
consEmpleado    c       =  (E c emptyS )

-- Propósito : indica el CUIL de un empleado.
-- Costo : O(1)
cuil :: Empleado -> CUIL
cuil    (E c _)     =   c 


-- Propósito : incorpora un sector al conjunto de sectores en los que trabaja un empleado.
-- Costo : O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector :: SectorId -> Empleado    -> Empleado
incorporarSector    s           (E c ss)    =  (E c (addS s ss)) 


-- Propósito : indica los sectores en los que el empleado traba ja.
-- Costo : O(S)
sectores :: Empleado    -> [SectorId]
sectores    (E c ss)    =  setToList ss 
