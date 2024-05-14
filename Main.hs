module Main where

import System.Environment
import System.IO

import Grammar.Par

import Interpreter (interpret)
import TypeChecker (typeCheck)

runInterpreter :: String -> IO ()
runInterpreter s = case pProgram $ myLexer s of
    Left err ->  hPutStrLn stderr $ "Parsing error: " ++ err
    Right parsed -> case typeCheck parsed of
        Left err -> hPutStrLn stderr $ "Static error: " ++ show err
        Right _ -> interpret parsed

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= runInterpreter
        file -> mapM_ (\f -> readFile f >>= runInterpreter) file