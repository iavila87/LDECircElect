--module Eval1elect (eval) where
module Eval1elect where

import ASTelec
import Stringcirc
import Control.Monad (guard)


-- Estados
-- type State = Either Error [(NVar,Integer)]
data Error = UndefVar | DivByZero deriving (Show, Eq)
type State = Either Error ([(NVar,Integer)], StateCirc) ---- cambios para introducir el estado del circuito

-- Estado inicial
initState :: State
initState = let s  = update "coordX" 0 ([],initStateCirc)
                s' = update "coordY" 0 s
            in Right s'

-- Cambia el valor de una variable en un estado. Devuelve un estado sin Either
updateCirc :: String -> ([(NVar,Integer)], StateCirc) -> ([(NVar,Integer)], StateCirc)
updateCirc txt (s,"") = (s,txt)
updateCirc txt (s,scirc) = (s,scirc ++ txt)

-- Busca el valor de una variable en un estado
lookfor :: NVar -> State -> Either Error Integer
lookfor var (Right s) = lookfor' var s

lookfor' :: NVar -> ([(NVar,Integer)], StateCirc) -> Either Error Integer
lookfor' var ([],sc) = Left UndefVar
lookfor' var (((x,y):xs, sc))= if var == x then Right y

-- Cambia el valor de una variable en un estado
update :: NVar -> Integer -> ([(NVar,Integer)], StateCirc) -> ([(NVar,Integer)], StateCirc)
update var valor ([],sc) = ([(var,valor)],sc)
update var valor ((x,y):xs,sc) = if var == x then ((var,valor):xs,sc)

-- Evalúa un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Devuelve el contenido de un Right
unRight (Right x) = x

-- Evalúa un comando en un estado dado
evalComm :: Comm -> State -> State
evalComm Skip e = e
evalComm (Let var expInt) s = let valor = evalIntExp expInt s
                              in case valor of
                                    Right v -> Right (update var v (unRight s))
                                    Left error -> Left error

evalComm (LetCirc varRes varCap varAmp expCirc) s = let s1 = evalCirc expCirc s
                                                        case s1 of 
                                                          Right s -> restotal = case (resistenciaTotal expCirc) of
                                                                                  Nothing -> 0
                                                                                  Just x -> x
                                                        s2 = Right (update varRes restotal (unRight s1))
                                                        captotal = case (capacidadTotal expCirc) of
                                                                        Nothing -> 0
                                                                        Just x -> x
                                                        s3 = Right (update varCap captotal (unRight s2))
                                                        amptotal = case (ampTotal (findSource expCirc) expCirc) of
                                                                      Nothing -> 0
                                                                      Just x -> x 
                                                    in Right (update varAmp amptotal (unRight s3))

evalComm (Seq Skip c1) s = evalComm c1 s
evalComm (Seq c1 c2) s = let s' = evalComm c1 s
                                  in evalComm (Seq Skip c2) s'
evalComm (Cond b c1 c2) s = let b1 = evalBoolExp b s
                            in case b1 of
                                Right True -> evalComm c1 s
                                Right False -> evalComm c2 s 
                                Left error -> Left error
evalComm (Repeat c b) s = evalComm (Seq c (Cond b Skip (Repeat c b))) s


-- evalComm para circuitos electronicos
"""
evalCirc c s = let xIni  = lookfor "coordX" s
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
                   -- Cálculo de la resistencia total del circuito y armado del string
                   rtotal = msgRes (resistenciaTotal c)
                   -- Cálculo de la capacitancia del circuito y armado del string
                   captotal = msgCap (capacidadTotal c)
                   -- Cálculo del amperaje del circuito y armado del string
                   atotal = msgAmpere (ampTotal (findSource c) c)
                   -- Actualizo y cierro la seccion de circuito de LATEX
               in updateCirc (str1 ++ str2 ++ str3 ++ str4 ++ endCirc ++ rtotal ++ captotal ++ atotal ++ endDoc) s1

"""
--evalCirc con errores
evalCirc c s =    let xIni  = lookfor "coordX" s
                              yIni  = lookfor "coordY" s
                              s1 = evalComm' c s
                              -- Valores de coordenadas luego de evalComm'
                              xFinEval = lookfor "coordX" s1
                              yFinEval = lookfor "coordY" s1
                              -- Cierre de circuito con el dibujo de GND
                              str1 = case xFinEval of
                                            Right n0 -> case yIni of
                                                                Right n1-> case yFinEval of
                                                                                    -- Decidimos agregar Right para no modificar retorno de strLine
                                                                                    Right n2 -> Right (strLine (strCoord n0 n1) (strCoord (n0) n2))
                                                                                    Left error -> Left error 
                                                                Left error -> Left error
                                            Left error -> Left error
                              
                              str2 = case xFinEval of
                                            Right x -> case yIni of
                                                            Right y -> Right (strLine (strCoord x y) (strCoord (x+2) y))
                                                            Left error -> Left error
                                            Left error -> Left error

                              str3 = case xFinEval of
                                            Right x -> case yIni of
                                                            Right y -> Right (strLine (strCoord (x+2) y) (strCoord (x+2) (y-2)))
                                                            Left error -> Left error
                                            Left error -> Left error

                              str4 = case xFinEval of
                                            Right x -> case yIni of
                                                            Right y -> Right (drawGND (x+2) (y-2))
                                                            Left error -> Left error
                                            Left error -> Left error

                              -- Cálculo de la resistencia total del circuito y armado del String
                              rtotal = msgRes (resistenciaTotal c)
                              -- Cálculo de la capacitancia del circuito y armado del String
                              captotal = msgCap (capacidadTotal c)
                              -- Cálculo del amperaje del circuito y armado del String
                              atotal = msgAmpere (ampTotal (findSource c) c)
                              -- Actualizo y cierro la seccion de circuito de LATEX
                  in case s1 of  
                          Right s -> Right (updateCirc (uRs str1 ++ uRs str2 ++ uRs str3 ++ uRs str4 ++ endCirc ++ rtotal ++ captotal ++ atotal ++ endDoc) s)
                          Left error -> Left error

--unRight para tipo de dato String
uRs (Right s) = s

--evalComm' con manejo de errores
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
                                          str6 = case xIni of
                                                        Right xInitial-> case yIni of
                                                                                    Right yInitial -> Right (strLine (strCoord xInitial yInitial) (strCoord xInitial (yInitial-2)))
                                                                                    Left error -> Left error
                                                        Left error -> Left error 
                      
                                          -- Actualizo coordX y coordY antes de evaluar el siguiente 
                                          -- componente del paralelo
                                          s4 = case xFinEvalC1 of
                                                    Right n -> Right (update "coordX" (n-2) (unRight s3))
                                                    Left error -> Left error
                                          s5 = case yFinEvalC1 of
                                                    Right n -> Right (update "coordY" (n-2) (unRight s4))
                                                    Left error -> Left error
                                          -- Línea vertical que une c1 con c2.
                                          -- Se actualiza para que no quede ninguna línea en blanco
                                          -- Línea vertical
                                          str3 = case xIni of
                                                        Right xInitial ->case yFinEvalC1 of
                                                                                Right yFinal -> Right (strLine (strCoord xInitial yFinal) (strCoord xInitial (yFinal-2)))
                                                                                Left error -> Left error 
                                                        Left error -> Left error                        

                                          -- Evaluación de c2
                                          s6 = evalComm' c2 s5
                                          xFinEvalC2  = lookfor "coordX" s6
                                          yFinEvalC2  = lookfor "coordY" s6
                                          -- Linea horizontal en la linea de c2
                                          str4 = case xIni of
                                                        Right xInitial ->case yFinEvalC1 of
                                                                                Right yFinal -> case xFinEvalC1 of
                                                                                                      Right xFinal -> Right (strLine (strCoord xInitial (yFinal-2)) (strCoord (xFinal-2) (yFinal-2)))
                                                                                                      Left error -> Left error
                                                                                Left error -> Left error 
                                                        Left error -> Left error  
                                                                                      --str4 = strLine (strCoord xIni(xInitial) (yFinEvalC1(yFinal)-2)) (strCoord (xFinEvalC1(xFinal)-2) (yFinEvalC1-2))
                                          -- Linea horizontal en la linea de c1
                                          str5 = case xFinEvalC1 of
                                                        Right xF1 -> case yIni of 
                                                                            Right yInitial -> case xFinEvalC2 of 
                                                                                                    Right xF2 -> Right (strLine (strCoord xF1 yInitial) (strCoord xF2 yInitial))
                                                                                                    Left error -> Left error
                                                                            Left error -> Left error
                                                        Left error -> Left error
                                          -- linea vertical
                                          str7a = case xFinEvalC1 of 
                                                            Right xfE -> case yIni of 
                                                                                Right yInitial-> case yFinEvalC2 of 
                                                                                                      Right yfE -> Right (strLine (strCoord xfE yInitial) (strCoord xfE yfE))
                                                                                                      Left error -> Left error
                                                                                Left error -> Left error
                                                            Left error -> Left error 
                                            
                                        
                                          str7b =case xFinEvalC2 of 
                                                            Right xfE -> case yFinEvalC2 of 
                                                                                Right yfE2-> case yFinEvalC1 of 
                                                                                                      Right yfE1 -> Right (strLine (strCoord xfE yfE2) (strCoord xfE yfE1))
                                                                                                      Left error -> Left error
                                                                                Left error -> Left error
                                                            Left error -> Left error 
                                        
                                          -- Comparando coordenadas según longitud del paralelo.
                                      in case xFinEvalC1 of
                                                Right c1 -> case xFinEvalC2 of
                                                                    Right c2 -> if (c1 > c2) then
                                                                                    Right (updateCirc (uRs str3 ++ uRs str4 ++ uRs str5 ++ uRs str6 ++ uRs str7a) (unRight s6))
                                                                                else
                                                                                    Right (updateCirc (uRs str3 ++ uRs str4 ++ uRs str5 ++ uRs str6 ++ uRs str7b) (unRight s6)) 
                                                                    Left error -> Left error
                                                Left error -> Left error
                    
                    -- Empiezo a dibujar los componentes
                    CompExpr (Source (Const n)) -> let x  = lookfor "coordX" s
                                                       y  = lookfor "coordY" s
                                                       s1 = case x of
                                                                    Right n -> Right (update "coordX" (n+2) (unRight s))
                                                                    Left error -> Left error
                                                       s2 = case y of 
                                                                 Right n ->Right (update "coordY" n (unRight s1))
                                                                 Left error -> Left error
                                                       --Actualizo el estado para el próximo componente pero no cambio coordenadas del actual
                                                       str1 =  case x of 
                                                                    Right x1-> case y of
                                                                                    Right y1-> Right (drawSource x1 y1 n)
                                                                                    Left error -> Left error
                                                                    Left error -> Left error
                                                       str2 = case x of 
                                                                    Right x1-> case y of
                                                                                    Right y1-> Right (strLine (strCoord x1 y1) (strCoord (x1+2) y1))
                                                                                    Left error -> Left error
                                                                    Left error -> Left error
                                                        
                                                       str3 = case x of 
                                                                    Right x1-> case y of
                                                                                    Right y1-> Right(drawGND x1 (y1-2))
                                                                                    Left error -> Left error
                                                                    Left error -> Left error
                                                        
                                                       -- Actualizo el string de latex
                                                   in  Right (updateCirc (uRs str1 ++ uRs str2 ++ uRs str3) (unRight s2))

                    CompExpr Voltmeter -> let x  = lookfor "coordX" s
                                              y  = lookfor "coordY" s
                                              s1 = case x of 
                                                        Right n-> Right (update "coordX" (n+2) (unRight s))
                                                        Left error -> Left error
                                              s2 = case y of
                                                        Right n-> Right (update "coordY" n (unRight s1))
                                                        Left error -> Left error
                                              str1 =  case x of 
                                                           Right x1-> case y of
                                                                           Right y1-> Right (strLine (strCoord x1 y1) (strCoord (x1+2) y1))
                                                                           Left error -> Left error
                                                           Left error -> Left error
                                                
                                                
                                                
                                              str2 = case x of 
                                                           Right x1-> case y of
                                                                           Right y1-> Right (drawVoltimeter (x1+1) y1)
                                                                           Left error -> Left error
                                                           Left error -> Left error
                                                
                                              str3 = case x of 
                                                           Right x1-> case y of
                                                                           Right y1-> Right  (drawGND (x1 +1)(y1-2))
                                                                           Left error -> Left error
                                                           Left error -> Left error
                                            
                                          in  Right (updateCirc ( uRs str1 ++ uRs str2 ++ uRs str3) (unRight s2))

                    CompExpr Amperemeter -> let x  = lookfor "coordX" s
                                                y  = lookfor "coordY" s
                                                -- Actualizo coordenadas para dibujar el próximo componente
                                                s1 = case x of 
                                                            Right n -> Right (update "coordX" (n+2) (unRight s))
                                                            Left error -> Left error
                                                s2 = case y of 
                                                            Right n -> Right (update "coordY" (n) (unRight s1))
                                                            Left error -> Left error
                                                --Dibujo amperímetro desde coordenadas iniciales
                                                str1 = case x of
                                                            Right n -> case y of 
                                                                            Right n1 -> Right (drawAmperemeter n n1)
                                                                            Left error -> Left error
                                                            Left error -> Left error
                                                --Actualizo string de Latex
                                            in  Right (updateCirc (uRs str1) (unRight s2))

                    CompExpr c1 -> let x  = lookfor "coordX" s
                                       y  = lookfor "coordY" s
                                       s1 = case x of
                                                    Right n -> Right (update "coordX" (n+2) (unRight s))
                                                    Left error -> Left error
                                       s2 = case y of 
                                                    Right n -> Right (update "coordY" (n) (unRight s1))
                                                    Left error -> Left error
                                       --Dibujo componente desde coordenadas iniciales
                                       str1 = case x of
                                                    Right x1 -> case y of
                                                                        Right y1 -> Right (drawComponent x1 y1 c1)
                                                                        Left error -> Left error
                                                    Left error -> Left error
                                       --Actualizo String de Latex
                                   in  Right (updateCirc (uRs str1) (unRight s2))

-- Función que retorna un string para dibujar una linea en Latex
strLine coord1 coord2 = "\\draw" ++ coord1 ++ lineCirc ++ coord2 ++ ";\n"

-- Función que devuelve el mayor
isX1Mayor :: Ord a => a -> a -> Bool
isX1Mayor n m = if n < m then False else True
--
cnv :: Show a => a -> String
cnv n = show n

--
strCoord :: (Show a1, Show a2) => a1 -> a2 -> [Char]
strCoord x y = " ("++ (cnv x) ++","++ (cnv y) ++") "

strComp :: Circ -> [Char]
strComp (Add (Resistance (Const r)) c) = ("to[R={" ++ (show r) ++ "}{ ohm},-]")

strComp (Add (Capacitance (Const ca)) c) = ("to[C={" ++ (show ca) ++ "}{ uF},-]")

strComp (Add Ohmmeter c) = "to[ohmmeter]"

strComp (Add Amperemeter c) = "to [rmeterwa, t=A, i=$i$]"

strComp (Add Voltmeter c) = "to[rmeterwa, t=V, v=$v$]"

strComp (Add (Switch BTrue) c) = "to[ccsw, -, name=s1]"

strComp (Add (Switch BFalse) c) = "to[cosw, -, name=s1]"

strComp (Add (Source (Const r)) c) = "to[battery1={" ++ (show r) ++ "}{V}]"

--Función para devolver concatenado el string que dibuja en GND
drawGND x y = "\\draw" ++ (strCoord x y) ++ gndCirc ++ ";\n"

--Función para devolver concatenado el string que dibuja la Source
drawSource x y n = "\\draw" ++ (strCoord x y) ++ (strComp (Add (Source (Const n)) EmptyCirc)) ++ (strCoord (x) (y-2)) ++ ";\n"

--Función para devolver concatenado el string que dibuja el Voltímetro

drawVoltimeter x y = "\\draw" ++ (strCoord x y) ++ (strComp (Add Voltmeter EmptyCirc)) ++ (strCoord x (y-2)) ++ ";\n"

--Función para devolver concatenado el string que dibuja el Amperímetro
  
drawAmperemeter x y = "\\draw" ++ (strCoord (x) y) ++ (strComp (Add Amperemeter EmptyCirc)) ++ (strCoord (x +2) y) ++ ";\n"

--Función para devolver concatenado el string que dibuja el Componente
drawComponent x y c= "\\draw" ++ (strCoord (x) y) ++ (strComp (Add c EmptyCirc)) ++ (strCoord (x +2) y) ++ ";\n"

-- Evalúa una expresión entera
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const valor) estado = Right valor
evalIntExp (Var variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = let valor = evalIntExp expInt estado
                                    in case valor of
                                        Right n -> Right (-n)
                                        Left error -> Left error
evalIntExp (Plus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                         valor2 = evalIntExp exp2 estado
                                     in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 + n2)
                                                            Left error -> Left error
                                            Left error

evalIntExp (Minus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                      in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 - n2)
                                                            Left error -> Left error
                                            Left err
evalIntExp (Times exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                      in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 * n2)
                                                            Left error -> Left error
                                            Left err
evalIntExp (Div exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                    in case valor1 of
                                                Right n1 -> case valor2 of
                                                                    Right 0 -> Left DivByZero
                                                                    Right n2 -> Right (div n1 n2)


-- Evalua una expresión booleana

evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue estado = Right True
evalBoolExp BFalse estado = Right False
evalBoolExp (Eq exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 == n2)
                                                            Left error -> Left error
                                            Left error -> Left error

evalBoolExp (Lt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 < n2)
                                                            Left error -> Left error
                                            Left error -> Left error

evalBoolExp (Gt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 > n2)
                                                            Left error -> Left error
                                            Left error -> Left error

evalBoolExp (And exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                         valor2 = evalBoolExp exp2 estado
                                     in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 && n2)
                                                            Left error -> Left error
                                            Left error -> Left error

evalBoolExp (Or exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                        valor2 = evalBoolExp exp2 estado
                                    in case valor1 of
                                            Right n1 -> case valor2 of
                                                            Right n2 -> Right (n1 || n2)
                                                            Left error -> Left error
                                            Left error -> Left error 
evalBoolExp (Not exp1) estado =     let b = evalBoolExp exp1 estado
                                    in case b of
                                            Right b1 -> Right (not b1)
                                            Left error -> Left error
                                            
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
resistenciaTotal (Add comp c) = Just (resistenciaComponente comp)

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
--msgRes::Maybe->String
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
capacidadTotal (Add comp c) = Just (capacidadComponente comp)

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
--ampTotal :: Integer -> Circ -> Integer
ampTotal v c = let rt = resistenciaTotal c 
               in case rt of 
                       Nothing -> Nothing
                       Just x -> Just (v `div` x)

--Evalúo si se puede calcular la intensidad de un circuito y retorno un mensaje en consecuencia
msgAmpere ap= case ap of 
            Nothing ->"\\\\\\\\No se puede calcular el amperaje total del circuito.\n"
            Just a -> "\\\\\\\\El amperaje total del circuito es de " ++ (cnv a) ++ "A.\n"

--Busco la Source en mi árbol. Mi Source va a estar a la izquierda
findSource (Add c _) = case c of
                               Source (Const v) -> v
                               _ -> 0
findSource (Serie l r) = findSource l
findSource (Parallel l r) = findSource l


