module Ast
  ( Expr (..),
    Stmt (..),
    Program,
    showProgram,
    reprProgram
  )
where

import Data.List (intercalate)
import Text.Printf (printf)

data Expr
  = Var String
  | Abs Char Expr
  | App Expr Expr

instance Show Expr where
  show (Var s) = s
  show (Abs c e) = '(' : 'λ' : c : '.' : show e ++ ")"
  show (App l r) = show l ++ ' ' : show r

data Stmt
  = Def String Expr
  | Expr Expr

instance Show Stmt where
  show (Def s e) = s ++ ":=" ++ show e
  show (Expr e) = show e

type Program = [Stmt]

showProgram :: Program -> String
showProgram program = intercalate "\n" $ map show program

reprExpr :: Expr -> String
reprExpr (Var s) = "Var s"
reprExpr (Abs c e) = printf "Abs %s (%s)" [c] (reprExpr e)
reprExpr (App l r) = printf "App (%s) (%s)" (reprExpr l) (reprExpr r)

reprStmt :: Stmt -> String
reprStmt (Def s e) = printf "Def %s (%s)" s (reprExpr e)
reprStmt (Expr e) = printf "Expr (%s)" (reprExpr e)

reprProgram :: Program -> String
reprProgram program = '[' : intercalate ", " (map reprStmt program) ++ "]"
