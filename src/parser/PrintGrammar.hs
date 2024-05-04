-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintGrammar.

module PrintGrammar where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsGrammar

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsGrammar.Ident where
  prt _ (AbsGrammar.Ident i) = doc $ showString i
instance Print (AbsGrammar.Program' a) where
  prt i = \case
    AbsGrammar.Start _ fndefs -> prPrec i 0 (concatD [prt 0 fndefs])

instance Print (AbsGrammar.FnDef' a) where
  prt i = \case
    AbsGrammar.FnDef _ type_ id_ argdecs block -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_, doc (showString "("), prt 0 argdecs, doc (showString ")"), prt 0 block])

instance Print [AbsGrammar.FnDef' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsGrammar.ArgDec' a) where
  prt i = \case
    AbsGrammar.ArgDec _ type_ id_ -> prPrec i 0 (concatD [prt 0 type_, prt 0 id_])

instance Print [AbsGrammar.ArgDec' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.Block' a) where
  prt i = \case
    AbsGrammar.Block _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsGrammar.Stmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsGrammar.Stmt' a) where
  prt i = \case
    AbsGrammar.Empty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsGrammar.Decl _ type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsGrammar.Ass _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsGrammar.Ret _ expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsGrammar.Cond _ expr block -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsGrammar.CondElse _ expr block1 block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block1, doc (showString "else"), prt 0 block2])
    AbsGrammar.While _ expr block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 block])
    AbsGrammar.Print _ expr -> prPrec i 0 (concatD [doc (showString "print"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString ";")])
    AbsGrammar.Println _ expr -> prPrec i 0 (concatD [doc (showString "println"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString ";")])
    AbsGrammar.FuncStmt _ fndef -> prPrec i 0 (concatD [prt 0 fndef])
    AbsGrammar.App _ expr -> prPrec i 0 (concatD [prt 0 expr])

instance Print (AbsGrammar.Item' a) where
  prt i = \case
    AbsGrammar.Item _ id_ expr -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 expr])

instance Print [AbsGrammar.Item' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.Type' a) where
  prt i = \case
    AbsGrammar.Int _ -> prPrec i 0 (concatD [doc (showString "int")])
    AbsGrammar.Str _ -> prPrec i 0 (concatD [doc (showString "string")])
    AbsGrammar.Bool _ -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsGrammar.Array _ type_ expr -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print (AbsGrammar.Expr' a) where
  prt i = \case
    AbsGrammar.EArr _ exprs -> prPrec i 6 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
    AbsGrammar.EArrVal _ id_ exprs -> prPrec i 6 (concatD [prt 0 id_, doc (showString "["), prt 0 exprs, doc (showString "]")])
    AbsGrammar.EVar _ id_ -> prPrec i 6 (concatD [prt 0 id_])
    AbsGrammar.EInt _ n -> prPrec i 6 (concatD [prt 0 n])
    AbsGrammar.ETrue _ -> prPrec i 6 (concatD [doc (showString "true")])
    AbsGrammar.EFalse _ -> prPrec i 6 (concatD [doc (showString "false")])
    AbsGrammar.EApp _ id_ args -> prPrec i 6 (concatD [prt 0 id_, doc (showString "("), prt 0 args, doc (showString ")")])
    AbsGrammar.EString _ str -> prPrec i 6 (concatD [printString str])
    AbsGrammar.Neg _ expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsGrammar.Not _ expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsGrammar.EMul _ expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsGrammar.EAdd _ expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsGrammar.ERel _ expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsGrammar.EAnd _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsGrammar.EOr _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])

instance Print [AbsGrammar.Expr' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.Arg' a) where
  prt i = \case
    AbsGrammar.ArgVal _ expr -> prPrec i 0 (concatD [prt 0 expr])
    AbsGrammar.ArgVar _ id_ -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id_])

instance Print [AbsGrammar.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.AddOp' a) where
  prt i = \case
    AbsGrammar.Plus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsGrammar.Minus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsGrammar.MulOp' a) where
  prt i = \case
    AbsGrammar.Times _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsGrammar.Div _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsGrammar.Mod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsGrammar.RelOp' a) where
  prt i = \case
    AbsGrammar.LTH _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsGrammar.LE _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsGrammar.GTH _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsGrammar.GE _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsGrammar.EQU _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsGrammar.NE _ -> prPrec i 0 (concatD [doc (showString "!=")])
