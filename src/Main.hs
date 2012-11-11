module Main(
  main
) where

import Control.Monad(liftM)
import System.Environment(getArgs)
import System.Console.GetOpt
import System.Exit(exitSuccess)

import LLang.AST
import LLang.Interpreter
import LLang.Printer
import LLang.Mix

data OperationMode = Int | Mix
data Options = Options { 
    mode :: OperationMode,
    inputFile :: FilePath
}

optionDescriptions :: [OptDescr (Options -> IO Options)]
optionDescriptions = [
    Option ['m'] ["mode"] 
        (ReqArg 
            (\arg opt -> case arg of 
              "mix" -> return opt { mode = Mix } 
              "int" -> return opt { mode = Int }
              _ -> putStrLn "Invalid value for 'mode' option. Use 'mix' or 'int'." 
                  >> exitSuccess)
            "STRING")
        "Mode of operation: 'mix' or 'int'"
  ]

processNonOptionArgs :: Options -> [String] -> IO Options
processNonOptionArgs opts [file] = return opts { inputFile = file }
processNonOptionArgs _ [] = printUsage >> exitSuccess
processNonOptionArgs _ _ = putStrLn "Exactly one input file required" >> exitSuccess

printUsage :: IO()
printUsage = putStrLn $ usageInfo header optionDescriptions
  where
  header = 
    "Usage: l-int [options] input-file\n" ++ 
    "Availible options:"


main :: IO ()
main = do
  args <- getArgs
  let (optFuns, inFiles, _) = getOpt RequireOrder optionDescriptions args 
  opts <- foldl (>>=) (return startOptions) optFuns >>= flip processNonOptionArgs inFiles
  input <- readFile $ inputFile opts
  processProgram (mode opts) $ readProgram $ lines input
    where
    startOptions = Options { mode = Int, inputFile = "-"}
    processProgram Int prg = int prg
    processProgram Mix prg = liftM printProgram (mix prg) >>= putStrLn

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
