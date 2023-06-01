module ASTelec where

-- Identificadores de Variable
type NVar = String

-- Expresiones Aritmeticas
data IntExp = Const Integer
            | Var NVar
            | UMinus IntExp
            | Plus IntExp IntExp
            | Minus IntExp IntExp
            | Times IntExp IntExp
            | Div IntExp IntExp
 deriving (Show,Eq)

-- Expresiones Booleanas
data BoolExp = BTrue
             | BFalse
             | Eq IntExp IntExp
             | Lt IntExp IntExp
             | Gt IntExp IntExp
             | And BoolExp BoolExp
             | Or BoolExp BoolExp
             | Not BoolExp
 deriving (Show,Eq)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm = Skip
          | Let NVar IntExp -- 
          | LetCirc NVar NVar NVar Circ 
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          -- | CircExpr Circ
 deriving (Show,Eq)

-- Expresiones Electronicas
data Circ = Serie Circ Circ
          | Parallel Circ Circ
          | Add Comp Circ      -- Agregar Pol Comp Circ  ---- Data Pol = Neg | Pos   -- -- | CompExpr Comp 
          | EmptyCirc          -- Circuito vacio
 deriving (Show,Eq) 

data Comp = Resistance IntExp
          | Capacitance IntExp
          | Source IntExp
          | Switch BoolExp
          | Voltmeter   -- Multimetro --
          | Amperemeter ----------------
          | Ohmmeter    ----------------
          | EmptyComp   -- Componente vacio
 deriving (Show, Eq)



