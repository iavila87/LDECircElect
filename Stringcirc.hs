module Stringcirc where

------ Estados Circuito
type StateCirc = String

------ Estado nulo para prueba de circuito
initStateCirc :: StateCirc
initStateCirc = usePackCirc1 ++ usePackCirc2 ++ usePackCirc3 ++ titAuthCirc ++ beginCirc


usePackCirc1 :: StateCirc
usePackCirc1 = "\\documentclass{article}\n\\usepackage[europeanresistors]{circuitikz}\n"

usePackCirc2 :: StateCirc
usePackCirc2 = "\\ctikzset{bipoles/thickness=0.8}\n\\ctikzset{bipoles/length=1cm}\n\\tikzstyle{every path}=[line width=1.25pt, line cap=round, line join=round]\n"

usePackCirc3 :: StateCirc
usePackCirc3 = "\\usepackage{lmodern}\n\\usepackage[T1]{fontenc}\n\\usepackage[spanish,activeacute]{babel}\n\\usepackage{mathtools}\n"

titAuthCirc :: StateCirc
titAuthCirc = "\\title{Circuito resultante}\n\\author{Avila - Torrazza}\n"

beginCirc :: StateCirc
beginCirc = "\\begin{document}\n\\maketitle\n% Mi primer documento en \\LaTeX{}.\n\\begin{circuitikz}[american]\n"
                 
endCirc :: StateCirc
endCirc = "\\end{circuitikz}\n"

endDoc = "\\end{document}\n"


gndCirc :: StateCirc
gndCirc = " node[ground](GND){} "

lineCirc :: StateCirc
lineCirc = " to[short] "