module LLang.Interpreter(
  int
) where

import System.IO(hFlush, stdout)
import Control.Monad

import LLang.AST
import qualified Data.Map.Strict as Map

type Environment = Map.Map Variable Integer

-- | Interprets programs
int :: Program -> IO ()
int (Program s) = void $ intStatement s emptyEnv
  where
  emptyEnv = Map.empty 

-- | Interprets statement in a given Environment
intStatement :: Statement -> Environment -> IO Environment
intStatement Skip env = return env
intStatement (Read var) env = do 
  n <- askForVar var
  return $ Map.insert var n env
    where 
    askForVar varName = do 
      putStr $ varName ++ " < "
      hFlush stdout
      readLn :: IO Integer
intStatement (Write expr) env = do
  putStrLn $ "  > " ++ show n
  return env
    where
    n = eval expr env
intStatement (Sequence s1 s2) env = 
  intStatement s1 env >>= intStatement s2
intStatement (Assign var expr) env =
  return $ Map.insert var value env
    where
    value = eval expr env
intStatement s@(While expr body) env = 
  case eval expr env of
    0 -> return env
    _ -> intStatement body env >>= intStatement s
intStatement (ITE cond th el) env =
  case eval cond env of
    0 -> intStatement el env
    _ -> intStatement th env

-- | Evaluates expression in a given Environment
eval :: Expression -> Environment -> Integer
eval (Constant n) _ = n
eval (VariableExpr v) env = val
  where
  (Just val) = Map.lookup v env
eval (Binary left op right) env = 
  applyOp op leftRes rightRes
    where
    leftRes = eval left env
    rightRes = eval right env
    applyOp "+" = (+)
    applyOp "-" = (-)
    applyOp "*" = (*)
    applyOp "/" = div
    applyOp "%" = rem
    applyOp "==" = \x y -> boolToInt $ x == y
    applyOp "!=" = \x y -> boolToInt $ x /= y
    applyOp "<" = \x y -> boolToInt $ x < y
    applyOp "<=" = \x y -> boolToInt $ x <= y
    applyOp ">" = \x y -> boolToInt $ x > y
    applyOp ">=" = \x y -> boolToInt $ x >= y
    boolToInt True = 1
    boolToInt False = 0
