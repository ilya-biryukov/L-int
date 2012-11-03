module LLang.Printer(
  printProgram
) where

import LLang.AST

printProgram :: Program -> String
printProgram (Program s) = 
  printInCurlyBraces s 0 ""

type Indent = Int
printIndent :: Indent -> ShowS
printIndent n = 
  printText $ replicate n ' '

printInCurlyBraces :: Statement -> Indent -> ShowS
printInCurlyBraces s ind = 
  printText "{\n" . printIndent (ind + 4) . 
  printStatement s (ind + 4) . printText "\n" . printIndent ind .
  printText "}"

printStatement :: Statement -> Indent -> ShowS
printStatement (Sequence s1 s2) ind =
  printStatement s1 ind . printText ";\n" . printIndent ind .
  printStatement s2 ind
printStatement (ITE cond th el) ind = 
  printText "if " . printExpression cond . printText " then\n" . printIndent ind .
  printInCurlyBraces th ind . printText "\n" . printIndent ind . 
  printText "else\n" . printIndent ind .
  printInCurlyBraces el ind
printStatement (While cond s) ind = 
  printText "whlie " . printExpression cond . printText " do\n" . printIndent ind .
  printInCurlyBraces s ind
printStatement s _ = printStatement' s

printStatement' :: Statement -> ShowS
printStatement' (Assign var expr) = 
  printText var . printText " := " . printExpression expr
printStatement' (Skip) =
  printText "skip"
printStatement' (Read var) = 
  printText "read(" . printText var . printText ")"
printStatement' (Write expr) = 
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
