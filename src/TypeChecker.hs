module TypeChecker where
-- Standard modules
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import System.IO
-- Grammar
import AbsGrammar
import Helper
----------------------------
------ Data and Types ------
----------------------------

data TType = TInt | TStr | TBool | TFun TType [TType] deriving Eq
instance Show TType where
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
-- TypeCheckerState --
type Env = Map.Map String TType
data TCS = TCS {
    env :: Env,
    ret1 :: Maybe TType,
    ret2 :: Maybe TType
}
initState = TCS { env = Map.empty, ret1 = Nothing, ret2 = Nothing }
-- TypeCheckerMonad --
type TCM a = ExceptT Err (State TCS) a
------------------------------
------ Helper Functions ------
------------------------------
getType :: Type -> TType
getType (Int _) = TInt
getType (Str _) = TStr
getType (Bool _) = TBool

insertType :: String -> TType -> TCM ()
insertType n t = modify (\s -> s { env = Map.insert n t (env s) })

-----------------------------------
-------- Check Functions ----------
-----------------------------------
checkType:: Pos -> TType -> TType -> TCM ()
checkType pos t1 t2 = 
    if t1 == t2 then return ()
    else throwError $ Err {
        pos=pos,
        reason="Types do not match. Expected type: " ++ show t1 ++ ". Actual type: " ++ show t2
        }

getArgType :: ArgDec -> TType
getArgType (ArgDec _ t _) = getType t
-----------------------------------
------ TypeChecker Functions ------
-----------------------------------

tcProg :: [FnDef] -> TCM ()
tcProg prog = do 
    mapM_ tcFnDef prog
    env <- gets env
    case Map.lookup "main" env of
        Just (TFun TInt []) -> return ()
        _ -> throwError $ Err {
            pos=Nothing,
            reason="Main function not found or has wrong type."
            }

tcFnDef:: FnDef -> TCM ()
tcFnDef (FnDef pos t n args block) = do
    insertType (getIdent n) (TFun (getType t) $ map getArgType args)
    state <- get
    mapM_ (\(ArgDec _ t_a n_a) -> insertType (getIdent n_a) (getType t_a)) args
    modify (\st -> st {ret1 = Just $ getType t})
    tcBlock block
    ret2 <- gets ret2
    case ret2 of 
        Just tr -> checkType pos (getType t) tr
        Nothing -> throwError $ Err {
            pos=pos,
            reason="Function " ++ getIdent n ++ " does not return a value."
            }
    put state

tcBlock:: Block -> TCM ()
tcBlock (Block _ block) = mapM_ tcStmt block

tcStmt:: Stmt -> TCM ()
tcStmt (Empty _) = return ()
tcStmt (Decl _ t items) = mapM_ (\(Item pos n exp) -> do
    let tt = getType t
    tcExp exp >>= checkType pos tt >> insertType (getIdent n) tt
    ) items
tcStmt (Ass pos n e) = do
    let name = getIdent n
    env <- gets env
    case Map.lookup name env of
        Just t -> (tcExp e) >>= checkType pos t
        Nothing -> throwError $ Err {
                    pos=pos,
                    reason="Variable " ++ name ++ " is not initialized."
                    }
tcStmt (Ret pos e) = do
    t <- tcExp e
    ret1 <- gets ret1
    case ret1 of
        Just tr -> checkType pos tr t
    modify (\st -> st {ret2 = Just t})
tcStmt (Cond pos e b) = do
    tcExp e >>= checkType pos TBool >> tcBlock b
tcStmt (CondElse pos e b1 b2) = do
    tcExp e >>= checkType pos TBool >> tcBlock b1 >> tcBlock b2
tcStmt (While pos e b) = do
    tcExp e >>= checkType pos TBool >> tcBlock b
tcStmt (Print _ _) = return ()
tcStmt (FuncStmt _ f) = do tcFnDef f

tcExp:: Expr -> TCM TType
tcExp (EVar pos n) = do
    let name = getIdent n
    env <- gets env 
    case Map.lookup name env of
        Just x -> return x
        Nothing -> throwError $ Err {
                    pos=pos,
                    reason="Variable " ++ name ++ " is not initialized."
                    }
-- tcExp (EArr _ _) TODO
-- tcExp (EArVal _ _) TODO
tcExp (EInt _ _) = return TInt
tcExp (ETrue _) = return TBool
tcExp (EFalse _) = return TBool
tcExp (EString _ _) = return TStr
tcExp (EApp pos n args) = do
    let name = getIdent n
    env <- gets env
    case Map.lookup name env of
        Just (TFun t ts) -> do
            argTypes <- mapM tcArg args
            if ts == argTypes then return t
            else throwError $ Err {
                pos=pos,
                reason="Function " ++ name ++ " called with wrong arguments."
                }
        Nothing -> throwError $ Err {
            pos=pos,
            reason="Function " ++ name ++ " is not defined."
            }
tcExp (Neg pos e) = do
    tcExp e >>= checkType pos TInt
    return TInt
tcExp (Not pos e) = do
    tcExp e >>= checkType pos TBool
    return TBool
tcExp (EMul pos e1 _ e2) = do
    tcExp e1 >>= checkType pos TInt
    tcExp e2 >>= checkType pos TInt
    return TInt
tcExp (EAdd pos e1 _ e2) = do
    tcExp e1 >>= checkType pos TInt
    tcExp e2 >>= checkType pos TInt
    return TInt
tcExp (ERel pos e1 _ e2) = do
    tcExp e1 >>= checkType pos TInt
    tcExp e2 >>= checkType pos TInt
    return TBool
tcExp (EAnd pos e1 e2) = do
    tcExp e1 >>= checkType pos TBool
    tcExp e2 >>= checkType pos TBool
    return TBool
tcExp (EOr pos e1 e2) = do
    tcExp e1 >>= checkType pos TBool
    tcExp e2 >>= checkType pos TBool
    return TBool

tcArg:: Arg -> TCM TType
tcArg (ArgVal _ e) = tcExp e
tcArg (ArgVar pos n) = do
    let name = getIdent n
    env <- gets env
    case Map.lookup name env of
        Just x -> return x
        Nothing -> throwError $ Err {
                    pos=pos,
                    reason="Variable " ++ name ++ " is not initialized."
                    }

runTCS s = runState s initState

runTCM :: TCM a -> (Either Err a, TCS)
runTCM = runTCS . runExceptT

typeCheck :: Program -> Either Err ()
typeCheck (Start _ prog) = case fst $ runTCM $ tcProg prog of
    Left e -> Left e
    Right _ -> Right ()