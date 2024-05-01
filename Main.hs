module Main where

import System.Environment
import System.IO

import ParGrammar

import Interpreter (interpret)
import TypeChecker (typeCheck)

run :: String -> IO ()
run s = case pProgram $ myLexer s of
    Left p_err ->  hPutStrLn stderr $ "Parsing error: " ++ p_err
    Right parsed -> case typeCheck parsed of
        Left t_err -> hPutStrLn stderr $ "Type check error: " ++ show t_err
        Right _ -> interpret parsed
        --Right _ -> hPutStrLn stdout $ "Bajo Jajo"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> getContents >>= run
        fs -> mapM_ (\f -> putStrLn ("======== " ++ f ++ " =======") >> readFile f >>= run) fs