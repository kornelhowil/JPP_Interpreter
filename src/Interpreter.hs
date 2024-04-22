module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import System.IO
-- Grammar
import AbsGrammar

interpret :: Program -> IO ()
interpret (Start _ prg) = do
    execStateT (runExceptT (catchError (execPrg prg) handleErr)) initState
    return ()