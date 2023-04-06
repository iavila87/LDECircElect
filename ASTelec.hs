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
            -- | Question BoolExp IntExp IntExp -- a > 0 ? 1 : 2;
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
          | Let NVar IntExp
          | Seq Comm Comm
          | Cond BoolExp Comm Comm
          | Repeat Comm BoolExp
          | CircExpr Circ -- agregamos circuitos
 deriving (Show,Eq)

-- Expresiones Electronicas
data Circ = Serie Circ Circ  -- data CircExpr = Serie CircExpr CircExpr
          | Parallel Circ Circ  -- | Parallel CircExpr CircExpr
          | CompExpr Comp 
 deriving (Show,Eq)

data Comp = Resistance IntExp
          | Capacitance IntExp
          | Source IntExp
          | Switch BoolExp
          | Voltmeter   -- Multimetro --
          | Amperemeter ----------------
          | Ohmmeter    ----------------
 deriving (Show, Eq)



