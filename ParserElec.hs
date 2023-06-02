module ParserElec where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import ASTelec

-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end",
                                                     "repeat",
                                                     "source", "parallel",
                                                     "serie", "voltmeter",
                                                     "ammeter", "ohmmeter",
                                                     "resistance", "capacitor",   -- emptycomp refiere a un componente vacio     
                                                     "switch", "emptycomp", "emptycirc"]}) -- emptycirc refiere al circuito no inicializado
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser IntExp
intexp  = chainl1 term addopp

term = chainl1 factor multopp

factor = try (parens lis intexp)
         <|> try (do reservedOp lis "-"
                     f <- factor
                     return (UMinus f))
         <|> (do n <- integer lis
                 return (Const n)
              <|> do str <- identifier lis
                     return (Var str))
                 
multopp = do try (reservedOp lis "*")
             return Times
          <|> do try (reservedOp lis "/")
                 return Div
 
addopp = do try (reservedOp lis "+")
            return Plus
         <|> do try (reservedOp lis "-")
                return Minus

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------
boolexp :: Parser BoolExp
boolexp  = chainl1 boolexp2 (try (do reservedOp lis "|"
                                     return Or))

boolexp2 = chainl1 boolexp3 (try (do reservedOp lis "&"
                                     return And))

boolexp3 = try (parens lis boolexp)
           <|> try (do reservedOp lis "~"
                       b <- boolexp3
                       return (Not b))
           <|> intcomp
           <|> boolvalue

intcomp = try (do i <- intexp
                  c <- compopp
                  j <- intexp
                  return (c i j))

compopp = try (do reservedOp lis "="
                  return Eq)
          <|> try (do reservedOp lis "<"
                      return Lt)
          <|> try (do reservedOp lis ">"
                      return Gt)

boolvalue = try (do reserved lis "true"
                    return BTrue)
            <|> try (do reserved lis "false"
                        return BFalse)

-----------------------------------
--- Parser de comandos
-----------------------------------
comm :: Parser Comm
comm = chainl1 comm2 (try (do reservedOp lis ";"
                              return Seq))


comm2 = try (do reserved lis "skip"
                return Skip)
        <|> try (do reserved lis "if"
                    cond <- boolexp
                    reserved lis "then"
                    case1 <- comm
                    reserved lis "else"
                    case2 <- comm
                    reserved lis "end"
                    return (Cond cond case1 case2))
        <|> try (do reserved lis "repeat"
                    c <- comm
                    reserved lis "until"
                    cond <- boolexp
                    reserved lis "end"
                    return (Repeat c cond))
        <|> try (do str <- identifier lis
                    reservedOp lis ":="
                    e <- intexp
                    return (Let str e))
        -- "(r1,c1,a1):==serie(paralello ) (resistance 4)"
        <|> try (do char '('
                    totalResistance <- identifier lis
                    char ','
                    totalCapacitance <- identifier lis
                    char ','
                    totalAmpere <- identifier lis
                    char ')'
                    reservedOp lis ":=="
                    c <- circexpr EmptyCirc
                    return (LetCirc totalResistance totalCapacitance totalAmpere c))

        -- <|> try (do c <- circexpr EmptyCirc
        --             return (CircExpr c))
        -- ver si es un circExpr y funciona o no

-----------------------------------
--- Parser de expresiones electronicas
-----------------------------------
circexpr :: Circ -> Parser Circ                   
circexpr prev = try (do reserved lis "serie"
                        char '('
                        c1 <- circexpr prev
                        char ')'
                        char '('
                        c2 <- circexpr prev
                        char ')'
                        return (Serie c1 c2))
                <|> try (do reserved lis "parallel"
                            char '('
                            c1 <- circexpr prev
                            char ')'
                            char '('
                            c2 <- circexpr prev
                            char ')'
                            return (Parallel c1 c2))
                <|> try (do reserved lis "emptycirc"
                            return (EmptyCirc))
                <|> try (do char '+'                  --polaridad directa
                            co <- comp
                            return (Add Pos co prev))
                <|> try (do char '-'                  --polaridad indirecta
                            co <- comp
                            return (Add Neg co prev))
        
        -----
                       
comp :: Parser Comp
comp    =  try (do reserved lis "resistance"
                   r <- intexp
                   return (Resistance r))
           <|> try (do reserved lis "capacitance"
                       cap <- intexp
                       return (Capacitance cap))
           <|> try (do reserved lis "source"
                       s <- intexp
                       return (Source s))
           <|> try (do reserved lis "switch"
                       sw <- boolexp
                       return (Switch sw))
           <|> try (do reserved lis "voltmeter"
                       v <- intexp
                       return (Voltmeter))
           <|> try (do reserved lis "ammeter"
                       a <- intexp
                       return (Amperemeter))
           <|> try (do reserved lis "ohmmeter"
                       o <- intexp
                       return (Ohmmeter))
           <|> try (do reserved lis "emptycomp"
                       return (EmptyComp))
--voltmeter", "ammeter", "ohmmeter
------------------------------------
-- Funcion de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm) 
