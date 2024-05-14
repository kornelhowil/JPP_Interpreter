module Helper where
import Grammar.Abs

type Pos = BNFC'Position

-- Error --
data Err = Err {
    pos :: Pos,
    reason :: String
}
instance Show Err where
    show err = case (pos err, reason err) of
        (Just (row, col), r) -> show row ++ ":" ++ show col ++ " " ++ r
        (_, r) -> r

getIdent :: Ident -> String
getIdent (Ident name) = name