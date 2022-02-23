module Ast
  ( Expr (..),
    Stmt (..),
    Program,
    showProgram
  )
where

import Data.List (intercalate)

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
