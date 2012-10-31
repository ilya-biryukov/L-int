module LLang.Printer(
  printProgram
) where

import LLang.AST

printProgram :: Program -> String
printProgram (Program s) = 
  printInCurlyBraces s ""

type Indent = Int
printIndent :: Indent -> ShowS
printIndent n = 
  printText $ take n $ repeat ' '

printInCurlyBraces :: Statement -> ShowS
printInCurlyBraces s = 
  printText "{\n" . 
  printStatement s . printText "\n" .
  printText "}\n"

printStatement :: Statement -> ShowS
printStatement (Assign var expr) = 
  printText var . printText " := " . printExpression expr
printStatement (Skip) =
  printText "skip"
printStatement (Sequence s1 s2) =
  printStatement s1 . printText ";\n" .
  printStatement s2
printStatement (ITE cond th el) = 
  printText "if " . printExpression cond . printText " then\n" .
  printInCurlyBraces th . 
  printText "else\n" .
  printInCurlyBraces el
printStatement (While cond s) = 
  printText "whlie " . printExpression cond . printText " do\n" .
  printInCurlyBraces s
printStatement (Read var) = 
  printText "read(" . printText var . printText ")"
printStatement (Write expr) = 
  printText "write(" . printExpression expr . printText ")"

printExpression :: Expression -> ShowS
printExpression (Binary left op right) = 
  printText "(" . printExpression left . printText ")" .
  printText op . 
  printText "(" . printExpression right . printText ")"
printExpression (Constant n) = (show n ++)
printExpression (VariableExpr var) = (var ++)
printExpression (Unary op expr) = 
  printText op . printExpression expr

printText :: String -> ShowS
printText s = (s++)
