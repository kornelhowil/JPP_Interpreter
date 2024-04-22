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
data TType = TInt | TStr | TBool | TFun TType [Arg] deriving Eq
instance Show TType where
    show TInt = "int"
    show TStr = "string"
    show TBool = "bool"
-- TypeCheckerState --
type Env = Map.Map String TType
data TCS = TCS {
    env :: Env,
    ret :: Maybe TType
}
initState = TCS { env = Map.empty, ret = Nothing }
-- TypeCheckerMonad --
type TCM a = ExceptT Err (State TCS) a
------------------------------
------ Helper Functions ------
------------------------------
takeType :: Type -> TType
takeType (Int _) = TInt
takeType (Str _) = TStr
takeType (Bool _) = TBool

insertType :: String -> TType -> TCM ()
insertType n t = modify (\s -> s { env = Map.insert n t (env s) })
{-
getType :: Pos -> String -> TCM TType
getType pos name = do
    env <- gets env 
    case Map.lookup name env of
        Just x -> return x
        Nothing -> throwError $ Err {
                    pos=pos,
                    reason="Variable " ++ name ++ " is not initialized."
                    }
-}
-----------------------------------
-------- Check Functions ----------
-----------------------------------
checkSameType:: Pos -> TType -> TType -> TCM ()
checkSameType pos t1 t2 = 
    if t1 == t2 then return ()
    else throwError $ Err {
        pos=pos,
        reason="Types do not match. Expected type: " ++ show t1 ++ "Actual type: " ++ show t2
        }
-----------------------------------
------ TypeChecker Functions ------
-----------------------------------
tcProg :: [Func] -> TCM ()
tcProg prog = mapM_ tcFunc prog

tcFunc:: Func -> TCM ()
tcFunc (FnDef _ t n args block) = do
    insertType (getIdent n) (TFun (takeType t) args)
    state <- get
    mapM_ (\arg -> case arg of
        ArgVal _ t_a n_a -> insertType (getIdent n_a) (takeType t_a)
        ArgVar _ t_a n_a -> insertType (getIdent n_a) (takeType t_a)
        ) args
    tcBlock block
    put state

tcBlock:: Block -> TCM ()
tcBlock (Block _ block) = mapM_ tcStmt block

tcStmt:: Stmt -> TCM ()
tcStmt (Empty _) = return ()
tcStmt (Decl _ t items) = mapM_ (\item -> do
    let tt = takeType t
    case item of 
        NoInit _ n -> insertType (getIdent n) tt
        Init pos n exp -> tcExp exp >>= checkSameType pos tt >> insertType (getIdent n) tt
    ) items

tcExp:: Expr -> TCM TType
--tcExp (EVar pos n) = getType pos (getIdent n)
tcExp (EVar pos n) = do
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