--module Eval1elect (eval) where
module Eval1elect where

import ASTelec
import Stringcirc
import Control.Monad (guard)

-- Errores
data Error = UndefVar | DivByZero deriving (Show, Eq)

-- Estados
-- type State = [(NVar,Integer)]
type State = Either Error ([(NVar,Integer)], StateCirc) ---- cambios para introducir el estado del circuito



-- Estado inicial
initState :: State
initState = let s  = update "coordX" 0 ([],initStateCirc)
                s' = update "coordY" 0 s
            in Right s'


-- Cambia el valor de una variable en un estado
updateCirc :: String -> State -> State
updateCirc txt (s,"") = (s,txt)
updateCirc txt (s,scirc) = (s,scirc ++ txt)


-- Busca el valor de una variable en un estado
lookfor :: NVar -> State -> Either Error Integer
lookfor var (Right s) = lookfor' var s

lookfor' :: NVar -> ([(NVar,Integer)], StateCirc) -> Either Error Integer
lookfor' var ([],sc) = Left UndefVar
lookfor' var (((x,y):xs, sc))= if var == x then Right y
                                           else lookfor' var (xs,sc)


-- Cambia el valor de una variable en un estado
--update :: NVar -> Integer -> State -> State -- antes
update :: NVar -> Integer -> ([(NVar,Integer)], StateCirc) -> ([(NVar,Integer)], StateCirc)
update var valor ([],sc) = ([(var,valor)],sc)
update var valor ((x,y):xs,sc) = if var == x then ((var,valor):xs,sc)
                                          else ((x,y): fst (update var valor (xs,sc)),sc)

-- Evalúa un programa en el estado inicial
eval :: Comm -> State
eval p = evalComm p initState

-- Devuelve el contenido de un Right
unRight (Right x) = x

-- Evalúa un comando en un estado dado
-- s representa estado
evalComm :: Comm -> State -> State
evalComm Skip s = s
                              -- con errores
evalComm (Let var expInt) s = let valor = evalIntExp expInt s
                              in case valor of
                                    Right v -> Right (update var v (unRight s))
                                    Left error -> Left error
                              -- sin errores
                              --let valor = evalIntExp expInt s
                              --in update var valor s


evalComm (Seq Skip c2) s = evalComm c2 s
evalComm (Seq c1 c2) s = let s' = evalComm c1 s
                                  in evalComm (Seq Skip c2) s'

                            -- con errores
evalComm (Cond b c1 c2) s = case (evalBoolExp b s) of
                                Right True -> evalComm c1 s
                                Right False -> evalComm c2 s 
                                Left error -> Left error
                            -- sin errores
                            --if (evalBoolExp b s )
                            --               then evalComm c1 s
                            --               else evalComm c2 s

evalComm (Repeat c b) s = evalComm (Seq c (Cond b Skip (Repeat c b))) s


-- evalComm para circuitos electronicos

evalComm (CircExpr c) s = let xIni  = lookfor "coordX" s
                              yIni  = lookfor "coordY" s
                              s1 = evalComm' c s
                              -- Valores de coordenadas luego de evalComm'
                              xFinEval = lookfor "coordX" s1
                              yFinEval = lookfor "coordY" s1
                              -- Cierre de circuito con el dibujo de GND
                              str1 = strLine (strCoord xFinEval yIni) (strCoord (xFinEval) yFinEval)
                              str2 = strLine (strCoord xFinEval yIni) (strCoord (xFinEval+2) yIni)
                              str3 = strLine (strCoord (xFinEval+2) yIni) (strCoord (xFinEval+2) (yIni-2))
                              str4 = drawGND (xFinEval +2) (yIni -2)
                              -- Cálculo de la resistencia total del circuito y armado del String
                              rtotal = msgRes (resistenciaTotal c)
                              -- Cálculo de la capacitancia del circuito y armado del String
                              captotal = msgCap (capacidadTotal c)
                              -- Cálculo del amperaje del circuito y armado del String
                              atotal = msgAmpere (ampTotal (findSource c) c)
                              -- Actualizo y cierro la seccion de circuito de LATEX
                          in updateCirc (str1 ++ str2 ++ str3 ++ str4 ++ endCirc ++ rtotal ++ captotal ++ atotal ++ endDoc) s1


evalComm' :: Circ -> State -> State
evalComm' c s = case c of
                    Serie c1 c2 -> let s1 = evalComm' c1 s
                                   in  evalComm' c2 s1
                          
                    
                    Parallel c1 c2 -> let xIni  = lookfor "coordX" s
                                          yIni  = lookfor "coordY" s
                                          -- Evaluación de c1
                                          s3 = evalComm' c1 s
                                          xFinEvalC1  = lookfor "coordX" s3
                                          yFinEvalC1  = lookfor "coordY" s3
                                          -- Línea vertical que une c1 con c2
                                          str6 = strLine (strCoord xIni yIni) (strCoord xIni (yIni-2))
                                          -- Actualizo coordX y coordY antes de evaluar el siguiente 
                                          -- componente del paralelo
                                          s4 = update "coordX" (xFinEvalC1-2) s3
                                          s5 = update "coordY" (yFinEvalC1-2) s4
                                          -- Línea vertical que une c1 con c2.
                                          -- Se actualiza para que no quede ninguna línea en blanco
                                          -- Línea vertical
                                          str3 = strLine (strCoord xIni yFinEvalC1) (strCoord xIni (yFinEvalC1-2))
                                          -- Evaluación de c2
                                          s6 = evalComm' c2 s5
                                          xFinEvalC2  = lookfor "coordX" s6
                                          yFinEvalC2  = lookfor "coordY" s6
                                          -- Linea horizontal en la linea de c2
                                          str4 = strLine (strCoord xIni (yFinEvalC1-2)) (strCoord (xFinEvalC1-2) (yFinEvalC1-2))
                                          -- Linea horizontal en la linea de c1
                                          str5 = strLine (strCoord xFinEvalC1 yIni) (strCoord xFinEvalC2 yIni)
                                          -- 
                                          str7a = strLine (strCoord xFinEvalC1 yIni) (strCoord xFinEvalC1 yFinEvalC2)
                                          str7b = strLine (strCoord xFinEvalC2 yFinEvalC2) (strCoord xFinEvalC2 yFinEvalC1)
                                          -- Comparando coordenadas según longitud del paralelo.
                                      in if (xFinEvalC1 > xFinEvalC2) then
                                            updateCirc (str3 ++ str4 ++ str5 ++ str6 ++ str7a) s6
                                         else
                                            updateCirc (str3 ++ str4 ++ str5 ++ str6 ++ str7b) s6
                    
                    -- Empiezo a dibujar los componentes
                    CompExpr (Source (Const n)) -> let x  = lookfor "coordX" s
                                                       y  = lookfor "coordY" s
                                                       s1 = update "coordX" (x+2) s
                                                       s2 = update "coordY" y s1
                                                       --Actualizo el estado para el próximo componente pero no cambio coordenadas del actual
                                                       str1 = drawSource x y n 
                                                       str2 = strLine (strCoord x y) (strCoord (x+2) y)
                                                       str3 = drawGND x (y-2)
                                                       -- Actualizo el string de latex
                                                   in  updateCirc (str1 ++ str2 ++ str3) s2

                    CompExpr Voltmeter -> let x  = lookfor "coordX" s
                                              y  = lookfor "coordY" s
                                              s1 = update "coordX" (x+2) s
                                              s2 = update "coordY" y s1
                                              str1 = strLine (strCoord x y) (strCoord (x+2) y)
                                              str2 = drawVoltimeter (x+1) y
                                              str3 = drawGND (x +1)(y-2)
                                          in  updateCirc (str1 ++ str2 ++ str3) s2

                    CompExpr Amperemeter -> let x  = lookfor "coordX" s
                                                y  = lookfor "coordY" s
                                                -- Actualizo coordenadas para dibujar el próximo componente
                                                s1 = update "coordX" (x+2) s
                                                s2 = update "coordY" (y) s1
                                                --Dibujo amperímetro desde coordenadas iniciales
                                                str1 = drawAmperemeter x y
                                                --Actualizo string de Latex
                                            in  updateCirc (str1) s2

                    CompExpr c1 -> let x  = lookfor "coordX" s
                                       y  = lookfor "coordY" s
                                       s1 = update "coordX" (x+2) s
                                       s2 = update "coordY" (y) s1
                                       --Dibujo componente desde coordenadas iniciales
                                       str1 = drawComponent x y c1 
                                       --Actualizo String de Latex
                                   in  updateCirc (str1) s2

-- Función que retorna un String para dibujar una línea en Latex
strLine :: [Char] -> [Char] -> [Char]
strLine coord1 coord2 = "\\draw" ++ coord1 ++ lineCirc ++ coord2 ++ ";\n"

-- Función para convertir a String
cnv :: Show a => a -> String
cnv n = show n

-- Función para retornar un String con la tupla de coordenadas
strCoord :: (Show a1, Show a2) => a1 -> a2 -> [Char]
strCoord x y = " ("++ (cnv x) ++","++ (cnv y) ++") "

-- Función que retorna el String para dibujar un componente específico en latex
strComp :: Circ -> [Char]
strComp (CompExpr (Resistance (Const r))) = ("to[R={" ++ (show r) ++ "}{ ohm},-]")
strComp (CompExpr (Capacitance (Const c))) = ("to[C={" ++ (show c) ++ "}{ uF},-]")
strComp (CompExpr Ohmmeter) = "to[ohmmeter]"
strComp (CompExpr Amperemeter) = "to [rmeterwa, t=A, i=$i$]"
strComp (CompExpr Voltmeter) = "to[rmeterwa, t=V, v=$v$]"
strComp (CompExpr (Switch BTrue)) = "to[ccsw, -, name=s1]"
strComp (CompExpr (Switch BFalse)) = "to[cosw, -, name=s1]"
strComp (CompExpr (Source (Const r))) = "to[battery1={" ++ (show r) ++ "}{V}]"

--Función para devolver concatenado el String que dibuja en GND
drawGND :: (Show a1, Show a2) => a1 -> a2 -> [Char]
drawGND x y = "\\draw" ++ (strCoord x y) ++ gndCirc ++ ";\n"

--Función para devolver concatenado el String que dibuja la Source
drawSource :: (Show a1, Show a2, Num a2) => a1 -> a2 -> Integer -> [Char]
drawSource x y n = "\\draw" ++ (strCoord x y) ++ (strComp (CompExpr (Source (Const n)))) ++ (strCoord (x) (y-2)) ++ ";\n"

--Función para devolver concatenado el String que dibuja el Voltímetro
drawVoltimeter :: (Show a1, Show a2, Num a2) => a1 -> a2 -> [Char]
drawVoltimeter x y = "\\draw" ++ (strCoord x y) ++ (strComp (CompExpr Voltmeter)) ++ (strCoord x (y-2)) ++ ";\n"

--Función para devolver concatenado el String que dibuja el Amperímetro
drawAmperemeter :: (Show a1, Show a2, Num a1) => a1 -> a2 -> [Char]
drawAmperemeter x y = "\\draw" ++ (strCoord (x) y) ++ (strComp (CompExpr Amperemeter)) ++ (strCoord (x +2) y) ++ ";\n"

--Función para devolver concatenado el String que dibuja el Componente
drawComponent :: (Show a1, Show a2, Num a1) => a1 -> a2 -> Comp -> [Char]
drawComponent x y c= "\\draw" ++ (strCoord (x) y) ++ (strComp (CompExpr c)) ++ (strCoord (x +2) y) ++ ";\n"

-- Evalúa una expresión entera
-- Completar definición
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const valor) estado = Right valor
evalIntExp (Var variable) estado = lookfor variable estado
                                    -- con errores
evalIntExp (UMinus expInt) estado = let valor = evalIntExp expInt estado
                                    in case valor of
                                        Right n -> Right (-n)
                                        Left error -> Left error
                                    -- sin errores
                                    --let valor = evalIntExp expInt estado
                                    --in (-valor)
                                     -- con errores
evalIntExp (Plus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                         valor2 = evalIntExp exp2 estado
                                     in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 + n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                     -- sin errores
                                     --let valor1 = evalIntExp exp1 estado
                                     --    valor2 = evalIntExp exp2 estado
                                     --in valor1 + valor2
                                      -- con errores
evalIntExp (Minus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                      in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 - n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                      -- sin errores  
                                      --let valor1 = evalIntExp exp1 estado
                                      --    valor2 = evalIntExp exp2 estado
                                      --in valor1 - valor2
                                     -- con errores
evalIntExp (Times exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                      in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 * n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                        -- sin errores
                                        --let valor1 = evalIntExp exp1 estado
                                        --    valor2 = evalIntExp exp2 estado
                                        --in valor1 * valor2
                                    -- con errores
evalIntExp (Div exp1 exp2) estado = case (evalIntExp exp1 estado) of
                                        Right n1 -> case (evalIntExp exp2 estado) of
                                                        Right 0 -> Left DivByZero
                                                        Right n2 -> Right (div n1 n2)
                                                        Left error -> error -- error -> error
                                        Left error -> Left error -- error -> error
                                    
                                        -- Sin errores
                                        --let valor1 = evalIntExp exp1 estado
                                        --    valor2 = evalIntExp exp2 estado
                                        --in div valor1 valor2

-- Evalua una expresion booleana
-- Completar definición
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue estado = Right True
evalBoolExp BFalse estado = Right False
                                    -- con errores
evalBoolExp (Eq exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 == n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                    -- sin errores
                                    --let valor1 = evalIntExp exp1 estado
                                    --    valor2 = evalIntExp exp2 estado
                                    --    in valor1 == valor2

                                    -- con errores
evalBoolExp (Lt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 < n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                    -- sin errores
                                    --let valor1 = evalIntExp exp1 estado
                                    --    valor2 = evalIntExp exp2 estado
                                    --    in valor1 < valor2

                                    -- con errores
evalBoolExp (Gt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 > n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                    -- sin errores
                                    --let valor1 = evalIntExp exp1 estado
                                    --    valor2 = evalIntExp exp2 estado
                                    --in valor1 > valor2

                                     -- con errores
evalBoolExp (And exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                         valor2 = evalBoolExp exp2 estado
                                     in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 && n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                     -- sin errores
                                     --let valor1 = evalBoolExp exp1 estado
                                     --    valor2 = evalBoolExp exp2 estado
                                     --   in valor1 && valor2

                                    -- con errores
evalBoolExp (Or exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                        valor2 = evalBoolExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 || n2)
                                                            Left error -> Left error
                                            Left error -> Left error
                                    -- sin errores
                                    --let valor1 = evalBoolExp exp1 estado
                                    --    valor2 = evalBoolExp exp2 estado
                                    --   in valor1 || valor2

                                -- con errores
evalBoolExp (Not exp1) estado = case (evalBoolExp exp1 estado) of
                                    Right b1 -> not b1
                                    Left error -> Left error
                                -- sin errores
                                -- not (evalBoolExp exp1 estado)

-- Defino la función para calcular la resistencia total de un circuito. Usamos tipo de dato Maybe para contemplar errores
resistenciaTotal :: Circ -> Maybe Integer
resistenciaTotal (Serie circ1 circ2) = do
                                        r1 <- resistenciaTotal circ1
                                        r2 <- resistenciaTotal circ2
                                        return (r1 + r2)
resistenciaTotal (Parallel circ1 circ2) = do
                                        r1 <- resistenciaTotal circ1
                                        r2 <- resistenciaTotal circ2
                                        guard ((r1 + r2) /= 0)  -- Agrego una guarda para evitar división por cero. Utilizo guard para verificar si la suma r1 + r2 es diferente de cero antes de realizar la división.
                                        return ((r1 * r2) `div` (r1 + r2))
resistenciaTotal (CompExpr comp) = Just (resistenciaComponente comp)

-- Defino la función auxiliar para obtener la resistencia de un componente
resistenciaComponente :: Comp -> Integer
resistenciaComponente (Resistance (Const v)) = v
resistenciaComponente (Capacitance _) = 0
resistenciaComponente (Source _) = 0
resistenciaComponente (Switch _) = 0
resistenciaComponente Voltmeter = 0
resistenciaComponente Amperemeter = 0
resistenciaComponente Ohmmeter = 0

--Evalúo si se puede calcular la resistencia de un circuito y retorno un mensaje en consecuencia
msgRes :: Show a => Maybe a -> [Char]
msgRes rt= case rt of 
            Nothing ->"\\\\\\\\No se puede calcular la resistencia del circuito.\n"
            Just r -> "\\\\\\\\La resistencia total del circuito es de " ++ (cnv r) ++ " ohm.\n"

-- Defino la función para calcular la capacitancia de un circuito. Usamos tipo de dato Maybe para contemplar errores
capacidadTotal :: Circ -> Maybe Integer
capacidadTotal (Serie circ1 circ2) = do
                                      c1 <- capacidadTotal circ1
                                      c2 <- capacidadTotal circ2
                                      guard ((c1 + c2) /= 0)  -- Agrego una guarda para evitar división por cero.
                                      return ((c1 * c2) `div` (c1 + c2))
capacidadTotal (Parallel circ1 circ2) = do
                                          c1 <- capacidadTotal circ1
                                          c2 <- capacidadTotal circ2
                                          return (c1 + c2)
capacidadTotal (CompExpr comp) = Just (capacidadComponente comp)

-- Defino la función auxiliar para obtener la capacitancia de un componente
capacidadComponente :: Comp -> Integer
capacidadComponente (Capacitance (Const v)) = v
capacidadComponente (Resistance _) = 0
capacidadComponente (Source _) = 0
capacidadComponente (Switch _) = 0
capacidadComponente Voltmeter = 0
capacidadComponente Amperemeter = 0
capacidadComponente Ohmmeter = 0

--Evalúo si se puede calcular la capacitancia de un circuito y retorno un mensaje en consecuencia
msgCap :: Show a => Maybe a -> [Char]
msgCap ct= case ct of 
            Nothing ->"\\\\\\\\No se puede calcular la capacitancia del circuito.\n"
            Just c -> "\\\\\\\\La capacitancia del circuito es de " ++ (cnv c) ++ " uF.\n"

{-
-- Detecta si es un capacitor el componente
isCapacitor c = case c of 
                    CompExpr (Capacitance x) -> True
                    CompExpr x -> False
                        
-- Busca si hay un capacitor en el circuito
findCap :: Circ -> Bool
findCap c = case c of 
                    CompExpr (Capacitance x) -> True
                    CompExpr x -> False
                    Serie c1 c2 -> (findCap c1) || (findCap c2)
                    Parallel c1 c2 -> (findCap c1) || (findCap c2)
-}

-- Calculo de la intensidad total del circuito.
--Intensidad= Tensión/ resistencia
ampTotal :: Integer -> Circ -> Maybe Integer
ampTotal v c = let rt = resistenciaTotal c 
               in case rt of 
                       Nothing -> Nothing
                       Just x -> Just (v `div` x)

--Evalúo si se puede calcular la intensidad de un circuito y retorno un mensaje en consecuencia
msgAmpere :: Show a => Maybe a -> [Char]
msgAmpere ap= case ap of 
            Nothing ->"\\\\\\\\No se puede calcular el amperaje total del circuito.\n"
            Just a -> "\\\\\\\\El amperaje total del circuito es de " ++ (cnv a) ++ "A.\n"

--Busco la Source en mi árbol. Mi Source va a estar a la izquierda
findSource :: Circ -> Integer
findSource (CompExpr c) = case c of
                               Source (Const v) -> v
                               _ -> 0
findSource (Serie l r) = findSource l
findSource (Parallel l r) = findSource l


