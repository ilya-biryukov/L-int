module Main(
  main
) where

import Control.Monad(liftM)
import System.Environment(getArgs)

import LLang.AST
import LLang.Interpreter
import LLang.Printer
import LLang.Mix

main :: IO ()
main = do
  args <- getArgs
  fname <- return $ args !! 0
  s <- readFile fname
  p <- return $ readProgram $ lines s
  mixed <- mix $ readProgram $ lines s
  putStrLn $ printProgram mixed


readProgram :: [String] -> Program
readProgram = Program . fst . readStatement
  where
  readStatement ("s": rest) = (Skip, rest)
  readStatement ("=": var: rest) = (Assign var expr, rest')
    where
    (expr, rest') = readExpr rest
  readStatement (";": rest) = (Sequence st1 st2, rest'')
    where
    (st1, rest') = readStatement rest
    (st2, rest'') = readStatement rest'
  readStatement ("r": var: rest) = (Read var, rest)
  readStatement ("w": rest) = (Write expr, rest')
    where
    (expr, rest') = readExpr rest
  readStatement ("i": rest) = (ITE cond th el, rest''')
    where
    (cond, rest') = readExpr rest
    (th, rest'') = readStatement rest' 
    (el, rest''') = readStatement rest''
  readStatement ("l": rest) = (While cond body, rest'')
    where
    (cond, rest') = readExpr rest
    (body, rest'') = readStatement rest'
  readExpr ("!": num: rest) = (Constant (read num), rest)
  readExpr ("x": var: rest) = (VariableExpr var, rest)
  readExpr ("@": op: rest) = (Binary left op right, rest'')
    where
    (left, rest') = readExpr rest
    (right, rest'') = readExpr rest'
