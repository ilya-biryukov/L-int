module LLang.AST(
  Operator,
  Variable,
  Expression(..), 
  Statement(..),
  Program(..)
) where

type Operator = String 

type Variable = String

data Expression = 
  Constant Integer | 
  VariableExpr Variable |
  Binary Expression Operator Expression

data Statement = 
  Skip |
  Read Variable |
  Write Expression |
  Assign Variable Expression |
  ITE { 
    ifCondifiton :: Expression,
    thenStatement :: Statement,
    elseStatement :: Statement
  } | 
  While Expression Statement | 
  Sequence Statement Statement

newtype Program = Program Statement
