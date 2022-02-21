module Ast
  ( Expr (..),
    Stmt (..),
    Program,
  )
where

data Expr
  = Var String
  | Abs Char Expr
  | App Expr Expr
  deriving (Show)

data Stmt
  = Def String Expr
  | Expr Expr
  deriving (Show)

type Program = [Stmt]
