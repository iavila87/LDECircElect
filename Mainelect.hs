module Main where

import System.Environment (getArgs)
import ParserElec (parseComm)
---- entrada salida
--import System.IO
-- Modificar este import para usar diferentes evaluadores
import Eval1elect
import ASTelec
import System.Process

-- Pruebas de salida en archivo
main :: IO ()
main = do arg:_ <- getArgs
          run arg

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile =
    do
        s <- readFile ifile
        case parseComm ifile s of
                                Left error -> print error
                                --Right t    -> print (eval t) --imprimir el resultado de evaluar.
                                Right t    -> (writeFile "circuitoutput.tex" (snd (eval t) ))
        
        -- s <- shortLinesOnly "pruebalatex.tex"
        -- putStrLn "Leyendo...creando archivo"
        -- escribe o crea un archivo
        --writeFile "testdecreacion.tex" (snd ( evalComm (CircExpr (Parallel ( Serie (CompExpr (Voltmeter)) (CompExpr (Resistance (Const 12))) ) (CompExpr (Resistance (Const 12))) )) initState ) )
        --ultimo utilizado
        --writeFile "testdecreacion.tex" (snd ( evalComm (CircExpr (Parallel ( Serie (CompExpr (Voltmeter)) (CompExpr (Resistance (Const 12))) ) (CompExpr (Resistance (Const 12))) )) initState ) )  
        --writeFile "testdecreacion.tex" (snd ( evalComm (CircExpr (Parallel ((Parallel ((CompExpr (Resistance (Const 12))) ) (CompExpr (Switch BTrue)) ) ) ((Parallel ((CompExpr (Resistance (Const 12))) ) (CompExpr (Switch BTrue)) ) ) )) initState ) )
        -- writeFile "testdecreacion.tex" (snd ( evalComm (CircExpr (Serie (CompExpr (Source (Const 12))) (CompExpr (Switch BFalse)) )) initState ) )
        putStrLn $ "se creo el documento."
        -- ejecuta un comando de sistema
        system "pdflatex circuitoutput.tex" >>= \exitCode -> print exitCode
        system "circuitoutput.pdf" >>= \exitCode -> print exitCode
