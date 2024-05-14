module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import System.IO
-- Grammar
import Grammar.Abs
import Helper

data Value = VInt Integer | VStr String | VBool Bool
instance Show Value where
    show v = case v of
        VInt x -> show x
        VStr x -> x
        VBool x -> if x then "true" else "false"

type Func = [Arg] -> IM Value

type Namespace = Map.Map String Int
type Values = Map.Map Int Value
type Functions = Map.Map Int Func
data IS = IS {
    names :: Namespace,
    vals :: Values,
    funcs :: Functions
}
type IM a = ExceptT Err (StateT IS IO) a
initState = IS { names = Map.empty, vals = Map.empty, funcs = Map.empty }

setNamespace :: Namespace -> IM ()
setNamespace names = modify (\s -> s { names = names })

insertFunc :: Int -> Func -> IM ()
insertFunc loc f = modify (\s -> s { funcs = Map.insert loc f (funcs s) })

insertName :: Int -> String -> IM ()
insertName loc n = modify (\s -> s { names = Map.insert n loc (names s) })

insertVal :: Int -> Value -> IM ()
insertVal loc v = modify (\s -> s { vals = Map.insert loc v (vals s) })

getLoc :: String -> IM Int
getLoc n = do
    ns <- gets names
    -- let (Just loc) = Map.lookup n ns
    case (Map.lookup n ns) of
        Just loc -> return loc
        Nothing -> throwError $ Err {
            pos=Nothing,
            reason= n ++ " not declared."
        }
    --return loc

getFunc :: Int -> IM Func
getFunc loc = do
    fs <- gets funcs
    let (Just func) = Map.lookup loc fs
    return func

getVal :: Int -> IM Value
getVal loc = do
    vs <- gets vals
    let (Just val) = Map.lookup loc vs
    return val

insertArg :: Namespace -> (ArgDec, Arg) -> IM ()
insertArg ns (ArgDec _ t n, ArgVal _ e) = do
    ns' <- gets names
    setNamespace ns'
    v <- evalExp e
    setNamespace ns
    loc <- newloc
    insertName loc $ getIdent n
    insertVal loc $ v
insertArg ns (ArgDec _ _ n1, ArgVar _ n2) = do
    let (Just loc) = Map.lookup (getIdent n2) ns
    insertName loc $ getIdent n2

newloc :: IM Int
newloc = do
    s <- get
    return $ Map.size (vals s) + Map.size (funcs s)

handleErr :: Err -> IM ()
handleErr err = do
    liftIO . hPutStrLn stderr $ "Runtime error: " ++ show err

evalMain :: IM ()
evalMain = do
    loc <- getLoc "main" 
    main <- getFunc loc
    main [] >> return ()

evalPrg :: [FnDef] -> IM ()
evalPrg prog = mapM_ evalFnDef prog >> evalMain

evalFnDef :: FnDef -> IM ()
evalFnDef (FnDef pos t n argdecs block) = do
    ns <- gets names
    loc <- newloc
    insertName loc $ getIdent n
    insertFunc loc (\args -> do
        ns' <- gets names
        mapM_ (insertArg ns') $ zip argdecs args
        res <- evalBlock block
        setNamespace ns'
        case res of
            Just v -> return v
            Nothing -> throwError $ Err {
                    pos=pos,
                    reason=(getIdent n) ++ " function does not return a value in all control paths."
                    }
        )

forBlock :: [Stmt] -> IM (Maybe Value)
forBlock [] = return Nothing
forBlock (stmt:stmts) = do
    res <- evalStmt stmt
    case res of
        Just v -> return $ Just v
        Nothing -> forBlock stmts

evalBlock :: Block -> IM (Maybe Value)
evalBlock (Block _ stmts) = do 
    ns <- gets names
    res <- forBlock stmts
    setNamespace ns
    return res 

evalStmt :: Stmt -> IM (Maybe Value)
evalStmt (Empty _) = return Nothing
evalStmt (Decl _ t items) = do
    mapM_ (\(Item pos n exp) -> do
        loc <- newloc
        insertName loc $ getIdent n
        v <- evalExp exp
        insertVal loc v
        ) items >> return Nothing
evalStmt (Ass _ n exp) = do
    loc <- getLoc $ getIdent n
    v <- evalExp exp
    insertVal loc v
    return Nothing
evalStmt (Ret _ exp) = do
    v <- evalExp exp
    return $ Just v
evalStmt (Cond _ exp b) = do
    VBool v <- evalExp exp
    if v then evalBlock b else return Nothing
evalStmt (CondElse _ exp b1 b2) = do
    VBool v <- evalExp exp
    if v then evalBlock b1 else evalBlock b2
evalStmt (While pos exp b) = do
    VBool v <- evalExp exp
    if v then do
        r <- evalBlock b
        case r of
            Nothing -> evalStmt (While pos exp b)
    else return Nothing
evalStmt (Print _ exp) = do
    evalExp exp >>= liftIO . putStr . show >> return Nothing
evalStmt (Println _ exp) = do
    evalExp exp >>= liftIO . putStrLn . show >> return Nothing
evalStmt (App _ exp) = do
    evalExp exp >> return Nothing
evalStmt (FuncStmt _ def) = do
    evalFnDef def >> return Nothing

evalExp :: Expr -> IM Value
evalExp (EVar _ n) = do
    loc <- getLoc $ getIdent n
    getVal loc
evalExp (EInt _ i) = return $ VInt i
evalExp (ETrue _) = return $ VBool True
evalExp (EFalse _) = return $ VBool False
evalExp (EString _ s) = return $ VStr s
evalExp (EApp _ n args) = do
    loc <- getLoc $ getIdent n
    func <- getFunc loc
    func args
evalExp (Neg _ e) = do
    (VInt x) <- evalExp e
    return $ VInt $ x * (-1)
evalExp (Not _ e) = do
    (VBool x) <- evalExp e
    return $ VBool $ not x
evalExp (EMul pos e1 op e2) = do
    (VInt x) <- evalExp e1
    (VInt y) <- evalExp e2
    case op of
        Times _ -> return $ VInt $ x * y
        Div _ -> case y of
            0 -> throwError $ Err {
                    pos=pos,
                    reason="Division by zero."
                    }
            _ -> return $ VInt $ x `div` y
        Mod _ -> case y of
            0 -> throwError $ Err {
                    pos=pos,
                    reason="Modulo by zero."
                    }
            _ -> return $ VInt $ x `mod` y
evalExp (EAdd _ e1 op e2) = do
    (VInt x) <- evalExp e1
    (VInt y) <- evalExp e2
    case op of
        Plus _ -> return $ VInt $ x + y
        Minus _ -> return $ VInt $ x - y
evalExp (ERel _ e1 op e2) = do
    (VInt x) <- evalExp e1
    (VInt y) <- evalExp e2
    case op of
        LTH _ -> return $ VBool $ x < y
        LE _ -> return $ VBool $ x <= y
        GTH _ -> return $ VBool $ x > y
        GE _ -> return $ VBool $ x >= y
        EQU _ -> return $ VBool $ x == y
        NE _ -> return $ VBool $ x /= y
evalExp (EAnd _ e1 e2) = do
    (VBool x) <- evalExp e1
    (VBool y) <- evalExp e2
    return $ VBool $ x && y
evalExp (EOr _ e1 e2) = do
    (VBool x) <- evalExp e1
    (VBool y) <- evalExp e2
    return $ VBool $ x || y


runIS s = execStateT s initState

runIM = runIS . runExceptT

interpret :: Program -> IO ()
interpret (Start _ prg) = do
    runIM (catchError (evalPrg prg) handleErr)
    return ()