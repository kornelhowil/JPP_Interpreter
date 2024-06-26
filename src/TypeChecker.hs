module TypeChecker where
-- Standard modules
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import System.IO
import qualified Data.Set as Set
-- Grammar
import Grammar.Abs
import Helper
----------------------------
------ Data and Types ------
----------------------------

data TType = TInt | TStr | TBool | TFun TType [TType] deriving Eq
instance Show TType where
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
    show (TFun t ts) = "Function(" ++ (show ts) ++ " -> " ++ (show t) ++ ")"
-- TypeCheckerState --
type Env = Map.Map String TType
data TCS = TCS {
    env :: Env,
    ret :: Maybe TType,
    argnames :: Set.Set String
}
initState = TCS { env = Map.empty, ret = Nothing, argnames = Set.empty}
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

checkPrint:: Pos -> TType -> TCM ()
checkPrint pos t = case t of
    TFun _ _ -> throwError $ Err {
        pos=pos,
        reason="Printing functions is not allowed"
        }
    _ -> return ()

checkArg:: Pos -> ArgDec -> TCM ()
checkArg pos (ArgDec _ _ n) = do
    s <- gets argnames
    if Set.member (getIdent n) s then throwError $ Err {
        pos=pos,
        reason="Argument " ++ (getIdent n) ++ " is duplicated."
        }
    else modify (\st -> st {argnames = Set.insert (getIdent n) s})
    return ()

checkArgs:: Pos -> [ArgDec] -> TCM ()
checkArgs pos args = do
    mapM_ (checkArg pos) args
    modify (\st -> st {argnames = Set.empty})
    return ()

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
    checkArgs pos args
    insertType (getIdent n) (TFun (getType t) $ map getArgType args)
    state <- get
    mapM_ (\(ArgDec _ t_a n_a) -> insertType (getIdent n_a) (getType t_a)) args
    modify (\st -> st {ret = Just $ getType t})
    tcBlock block
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
                    reason="Variable " ++ name ++ " is not declared."
                    }
tcStmt (Ret pos e) = do
    t <- tcExp e
    ret <- gets ret
    case ret of
        Just tr -> checkType pos tr t
tcStmt (Cond pos e b) = do
    tcExp e >>= checkType pos TBool >> tcBlock b
tcStmt (CondElse pos e b1 b2) = do
    tcExp e >>= checkType pos TBool >> tcBlock b1 >> tcBlock b2
tcStmt (While pos e b) = do
    tcExp e >>= checkType pos TBool >> tcBlock b
tcStmt (Print pos e) = tcExp e >>= checkPrint pos >> return ()
tcStmt (Println pos e) = tcExp e >>= checkPrint pos >> return ()
tcStmt (FuncStmt _ f) = tcFnDef f >> return ()
tcStmt (App pos e) = case e of
    EApp _ _ _ -> tcExp e >> return ()
    _ -> throwError $ Err {
                    pos=pos,
                    reason="This is not correct function call."
                    }

tcExp:: Expr -> TCM TType
tcExp (EVar pos n) = do
    let name = getIdent n
    env <- gets env 
    case Map.lookup name env of
        Just x -> return x
        Nothing -> throwError $ Err {
                    pos=pos,
                    reason="Variable " ++ name ++ " is not declared."
                    }
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
        Just _ -> throwError $ Err {
            pos=pos,
            reason="Variable " ++ name ++ " is not a function."
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
tcExp (EAdd pos e1 op e2) = do
    t1 <- tcExp e1
    t2 <- tcExp e2
    checkType pos t1 t2
    case t1 of
        TInt -> return TInt
        TStr -> case op of
            Plus _ -> return TStr
            Minus _ -> throwError $ Err {
                pos=pos,
                reason="Cannot subtract strings."
                }
        _ -> throwError $ Err {
            pos=pos,
            reason="Cannot add " ++ show t1 ++ " and " ++ show t2
            }
tcExp (ERel pos e1 _ e2) = do
    t1 <- tcExp e1
    t2 <- tcExp e2
    checkType pos t1 t2
    case t1 of
        TInt -> return TBool
        TStr -> return TBool
        TBool -> return TBool
        _ -> throwError $ Err {
            pos=pos,
            reason="Cannot compare " ++ show t1 ++ " and " ++ show t2
            }
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
                    reason="Variable " ++ name ++ " is not declared."
                    }

runTCS s = runState s initState

runTCM :: TCM a -> (Either Err a, TCS)
runTCM = runTCS . runExceptT

typeCheck :: Program -> Either Err ()
typeCheck (Start _ prog) = case fst $ runTCM $ tcProg prog of
    Left e -> Left e
    Right _ -> Right ()