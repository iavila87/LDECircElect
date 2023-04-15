--module Eval1elect (eval) where
module Eval1elect where

import ASTelec
import Stringcirc


-- Estados
-- type State = [(NVar,Integer)]
type State = ([(NVar,Integer)], StateCirc) ---- cambios para introducir el estado del circuito



-- Estado inicial
initState :: State
initState = let s  = update "coordX" 0 ([],initStateCirc)
                s' = update "coordY" 0 s
            in s'


-- Cambia el valor de una variable en un estado
updateCirc :: String -> State -> State
updateCirc txt (s,"") = (s,txt)
updateCirc txt (s,scirc) = (s,scirc ++ txt)





-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: NVar -> State -> Integer
lookfor var (((x,y):xs, sc))= if var == x then y
                                          else lookfor var (xs,sc)


-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: NVar -> Integer -> State -> State
update var valor ([],sc) = ([(var,valor)],sc)
update var valor ((x,y):xs,sc) = if var == x then ((var,valor):xs,sc)
                                          else ((x,y): fst (update var valor (xs,sc)),sc)

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip e = e
evalComm (Let var expInt) estado = let valor = evalIntExp expInt estado
                                       in update var valor estado
evalComm (Seq Skip c1) s = evalComm c1 s
evalComm (Seq c1 c2) s = let s' = evalComm c1 s
                                  in evalComm (Seq Skip c2) s'
evalComm (Cond b c1 c2) s = if (evalBoolExp b s )
                                           then evalComm c1 s
                                           else evalComm c2 s
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
                              str4 = "\\draw" ++ (strCoord (xFinEval+2) (yIni-2)) ++ gndCirc ++ ";\n"
                              -- Calculo de la resistencia total del circuito y armado del string
                              rtotal = "\\\\\\\\La resistencia total del circuito es de " ++ cnv (resTotal c) ++ " ohm.\n"
                              -- Actualizo y cierro la seccion de circuito de LATEX
                          in updateCirc (str1 ++ str2 ++ str3 ++ str4 ++ endCirc ++ rtotal ++ endDoc) s1


evalComm' :: Circ -> State -> State
evalComm' c s = case c of
                    Serie c1 c2 -> let s1 = evalComm' c1 s
                                       s2 = evalComm' c2 s1
                                   in  s2
                    
                    Parallel c1 c2 -> let xIni  = lookfor "coordX" s
                                          yIni  = lookfor "coordY" s
                                          -- Evaluacion de c1
                                          s3 = evalComm' c1 s
                                          xFinEvalC1  = lookfor "coordX" s3
                                          yFinEvalC1  = lookfor "coordY" s3
                                          -- linea vertical que une c1 con c2
                                          str6 = strLine (strCoord xIni yIni) (strCoord xIni (yIni-2))
                                          -- Actualizo coordX y coordY antes de evaluar el siguiente 
                                          -- componente del paralelo
                                          s4 = update "coordX" (xFinEvalC1-2) s3
                                          s5 = update "coordY" (yFinEvalC1-2) s4
                                          -- linea vertical que une c1 con c2.
                                          -- Se actualiza para que no quede ninguna línea en blanco
                                          str3 = strLine (strCoord xIni yFinEvalC1) (strCoord xIni (yFinEvalC1-2))
                                          -- Evaluacion de c2
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
                                          
                                      in if (isX1Mayor xFinEvalC1 xFinEvalC2) then
                                            updateCirc (str3 ++ str4 ++ str5 ++ str6 ++ str7a) s6
                                         else
                                            updateCirc (str3 ++ str4 ++ str5 ++ str6 ++ str7b) s6
                    
                    
                    CompExpr (Source (Const n)) -> let x  = lookfor "coordX" s
                                                       y  = lookfor "coordY" s
                                                       s1 = update "coordX" (x+2) s
                                                       s2 = update "coordY" y s1
                                                       --Actualizo el estado para el próximo componente pero no cambio coordenadas del actual
                                                       str1 = "\\draw" ++ (strCoord x y) ++ (strComp (CompExpr (Source (Const n)))) ++ (strCoord (x) (y-2)) ++ ";\n"
                                                       str2 = strLine (strCoord x y) (strCoord (x+2) y)
                                                       str3 = "\\draw" ++ (strCoord x (y-2)) ++ gndCirc ++ ";\n"
                                                   in  updateCirc (str1++str2++str3) s2 --(str1 ++ str2 ++ str3, s2)

                    CompExpr Voltmeter -> let x  = lookfor "coordX" s
                                              y  = lookfor "coordY" s
                                              s1 = update "coordX" (x+2) s
                                              s2 = update "coordY" y s1
                                              str1 = strLine (strCoord x y) (strCoord (x+2) y)
                                              str2 = "\\draw" ++ (strCoord (x+1) y) ++ (strComp (CompExpr Voltmeter)) ++ (strCoord (x+1) (y-2)) ++ ";\n"
                                              str3 = "\\draw" ++ (strCoord (x+1) (y-2)) ++ gndCirc ++ ";\n"
                                          in  updateCirc (str1++str2++str3) s2

                    CompExpr Amperemeter -> let x  = lookfor "coordX" s
                                                y  = lookfor "coordY" s
                                                s1 = update "coordX" (x+2) s
                                                s2 = update "coordY" (y) s1
                                                x1  = lookfor "coordX" s2
                                                y1  = lookfor "coordY" s2
                                                str1 = ("\\draw" ++ (strCoord (x) y) ++ (strComp (CompExpr Amperemeter)) ++ (strCoord (x1) y1) ++ ";\n")
                                            in  updateCirc (str1) s2

                    CompExpr c1 -> let x  = lookfor "coordX" s
                                       y  = lookfor "coordY" s
                                       s1 = update "coordX" (x+2) s
                                       s2 = update "coordY" (y) s1
                                       x1  = lookfor "coordX" s2
                                       y1  = lookfor "coordY" s2
                                       str1 = "\\draw" ++ (strCoord (x) y) ++ (strComp (CompExpr c1)) ++ (strCoord (x1) y1) ++ ";\n"
                                   in  updateCirc (str1) s2

-- Funcion que retorna un string para dibujar una linea en Latex
strLine coord1 coord2 = "\\draw" ++ coord1 ++ lineCirc ++ coord2 ++ ";\n"

-- Funcion que devuelve el mayor
isX1Mayor :: Ord a => a -> a -> Bool
isX1Mayor n m = if n < m then False else True
--
cnv :: Show a => a -> String
cnv n = show n

--
strCoord :: (Show a1, Show a2) => a1 -> a2 -> [Char]
strCoord x y = " ("++ (cnv x) ++","++ (cnv y) ++") "

strComp :: Circ -> [Char]
strComp (CompExpr (Resistance (Const r))) = ("to[R={" ++ (show r) ++ "}{ ohm},-]")

strComp (CompExpr (Capacitance (Const c))) = ("to[C={" ++ (show c) ++ "}{ uF},-]")

strComp (CompExpr Ohmmeter) = "to[ohmmeter]"

strComp (CompExpr Amperemeter) = "to [rmeterwa, t=A, i=$i$]"

strComp (CompExpr Voltmeter) = "to[rmeterwa, t=V, v=$v$]"

strComp (CompExpr (Switch BTrue)) = "to[ccsw, -, name=s1]"

strComp (CompExpr (Switch BFalse)) = "to[cosw, -, name=s1]"

strComp (CompExpr (Source (Const r))) = "to[battery1={" ++ (show r) ++ "}{V}]"

  
-- Evalua una expresion entera
-- Completar definicion
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const valor) estado = valor
evalIntExp (Var variable) estado = lookfor variable estado
evalIntExp (UMinus expInt) estado = let valor = evalIntExp expInt estado
                                    in (-valor)
evalIntExp (Plus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                         valor2 = evalIntExp exp2 estado
                                         in valor1 + valor2

evalIntExp (Minus exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                          in valor1 - valor2
evalIntExp (Times exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                          valor2 = evalIntExp exp2 estado
                                          in valor1 * valor2
evalIntExp (Div exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in div valor1 valor2


-- Evalua una expresion entera
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue estado = True
evalBoolExp BFalse estado = False
evalBoolExp (Eq exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 == valor2

evalBoolExp (Lt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 < valor2

evalBoolExp (Gt exp1 exp2) estado = let valor1 = evalIntExp exp1 estado
                                        valor2 = evalIntExp exp2 estado
                                        in valor1 > valor2

evalBoolExp (And exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                         valor2 = evalBoolExp exp2 estado
                                        in valor1 && valor2

evalBoolExp (Or exp1 exp2) estado = let valor1 = evalBoolExp exp1 estado
                                        valor2 = evalBoolExp exp2 estado
                                        in valor1 || valor2
evalBoolExp (Not exp1) estado = not (evalBoolExp exp1 estado)


-- Calculo de la resistencia total del circuito.
resTotal :: Circ -> Integer
resTotal circ = if (findCap circ) then -1
                else resTotal' circ

resTotal' circ = case circ of 
                         CompExpr (Resistance (Const x))    -> x
                         Serie (CompExpr Voltmeter) (c2)    -> resTotal' c2
                         Serie (CompExpr (Source n)) (c2)    -> resTotal' c2
                         Serie c1 c2        -> (resTotal' c1) + (resTotal' c2)
                         
                         Parallel c1 c2     -> ((resTotal' c1) * (resTotal' c2)) `div` ((resTotal' c1) + (resTotal' c2))
                         
-- Calculo de la capacidad total del circuito.
capTotal :: Circ -> Integer
capTotal circ = if (findCap circ) then capTotal' circ
                else -1

capTotal' circ = case circ of 
                         CompExpr (Capacitance (Const x))    -> x
                         
                         Serie c1 c2        -> ((capTotal' c1) * (capTotal' c2)) `div` ((capTotal' c1) + (capTotal' c2))
                         
                         Parallel c1 c2     -> (capTotal' c1) + (capTotal' c2)

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

-- Calculo de la intensidad total del circuito.
ampTotal :: Integer -> Circ -> Integer
ampTotal v c = v `div` (resTotal c)

