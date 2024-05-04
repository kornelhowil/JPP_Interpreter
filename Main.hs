module Main where

import System.Environment
import System.IO

import ParGrammar

import Interpreter (interpret)
import TypeChecker (typeCheck)

runInterpreter :: String -> IO ()
runInterpreter s = case pProgram $ myLexer s of
    Left p_err ->  hPutStrLn stderr $ "Parsing error: " ++ p_err
    Right parsed -> case typeCheck parsed of
        Left t_err -> hPutStrLn stderr $ "Static error: " ++ show t_err
        Right _ -> interpret parsed

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= runInterpreter
        file -> mapM_ (\f -> readFile f >>= runInterpreter) file