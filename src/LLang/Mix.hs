module LLang.Mix(
  mix
) where

import System.IO(hFlush, stdout)
import Data.Maybe

import LLang.AST
import qualified Data.Map.Strict as Map

data Value = 
  Static Integer |
  Dynamic

readValue :: String -> Value
readValue "_" = Dynamic
readValue s = Static (read s)

liftOp2 :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
liftOp2 _ Dynamic _ = Dynamic
liftOp2 _ _ Dynamic = Dynamic
liftOp2 f (Static x) (Static y) = Static $ f x y

type Environment = Map.Map Variable Value

emptyEnv :: Environment
emptyEnv = Map.empty 

lookupValue :: Environment -> Variable -> Value
lookupValue env var = fromMaybe Dynamic (Map.lookup var env) 

-- | Partially evaluates programs
mix :: Program -> IO Program
mix (Program s) = liftM (Program . snd) (mixStatement s emptyEnv)

-- | Partially evaluate a Statement in a given Environment
mixStatement :: Statement -> Environment -> IO (Environment, Statement)
mixStatement Skip env = return (env, Skip)
mixStatement s@(Read var) env = do 
  n <- askForVar var 
  readWithValue n
    where 
    askForVar varName = do 
      putStr $ varName ++ " < "
      hFlush stdout
      liftM readValue getLine 
    readWithValue n = return (newEnv, newStmt)
      where
      newEnv = Map.insert var n env
      newStmt = case n of 
        Static _ -> Skip
        Dynamic -> s 
mixStatement (Write expr) env = 
  return (env, Write newExpr)
    where
    (_, newExpr) = eval expr env
mixStatement (Sequence s1 s2) env = do
  (env', newStmt1) <- mixStatement s1 env
  (env'', newStmt2) <- mixStatement s2 env'
  return (env'', Sequence newStmt1 newStmt2)
mixStatement (Assign var expr) env =
  return (newEnv, newStmt)
    where
    (value, newExpr) = eval expr env
    newEnv = Map.insert var value env
    newStmt = case value of 
      Static _ -> Skip
      Dynamic -> Assign var newExpr
mixStatement s@(While expr body) env = 
  mixStatement (ITE expr thStmt Skip) env
    where
    thStmt = Sequence body s
mixStatement s@(ITE cond th el) env =
  let 
    (value, _) = eval cond env
    staticAssignments = Map.foldrWithKey addAssignment Skip env 
    addAssignment var (Static val) stmt = Sequence stmt (Assign var (Constant val))
    addAssignment _ Dynamic stmt = stmt
  in
    case value of
      Static 0 -> mixStatement el env
      Static _ -> mixStatement th env
      Dynamic -> return (emptyEnv, Sequence staticAssignments s)

eval :: Expression -> Environment -> (Value, Expression)
eval (Binary left op right) env = (val, newExpr)
    where
    val = applyOp op leftRes rightRes 
    newExpr = case val of
      Static n -> Constant n
      Dynamic -> Binary leftExpr op rightExpr
    (leftRes, leftExpr) = eval left env
    (rightRes, rightExpr) = eval right env
    applyOp "+" = liftOp2 (+)
    applyOp "-" = liftOp2 (-)
    applyOp "*" = liftOp2 (*)
    applyOp "/" = liftOp2 div
    applyOp "%" = liftOp2 rem
    applyOp "==" = liftOp2 (\x y -> boolToInt $ x == y)
    applyOp "!=" = liftOp2 (\x y -> boolToInt $ x /= y)
    applyOp "<" = liftOp2 (\x y -> boolToInt $ x < y)
    applyOp "<=" = liftOp2 (\x y -> boolToInt $ x <= y)
    applyOp ">" = liftOp2 (\x y -> boolToInt $ x > y)
    applyOp ">=" = liftOp2 (\x y -> boolToInt $ x >= y)
    boolToInt True = 1
    boolToInt False = 0
eval expr env = (val, newExpr)
  where
  val = eval' expr env
  newExpr = 
    case val of 
      Static n -> Constant n
      Dynamic -> expr

eval' :: Expression -> Environment -> Value
eval' (Constant n) _ = Static n
eval' (VariableExpr v) env = lookupValue env v
