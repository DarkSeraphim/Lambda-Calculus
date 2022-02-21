module Parser
  ( parse,
  )
where

import Ast
import Control.Monad
import Data.Functor
import Text.Parsec hiding (parse)

type Depth = Int

type Parser = Parsec String Depth

space' :: String
space' = " \t\r\f\v"

eol :: Parser ()
eol = (endOfLine $> ()) <|> eof

comment :: Parser String
comment = char ';' *> manyTill anyChar (lookAhead eol)

maybeEol :: Parser ()
maybeEol = do
  depth <- getState
  when (depth >= 1) (optional (endOfLine *> ws))

ws :: Parser ()
ws = many (oneOf space') *> optional comment *> maybeEol

wsn :: Parser ()
wsn = ws *> optional (endOfLine *> wsn)

symbol :: String -> Parser String
symbol s = string s <* ws

ident :: Parser String
ident = many1 (noneOf (space' ++ "\n;()")) <* ws

lambda :: Parser String
lambda = symbol "λ" <|> symbol "\\"

group :: Parser Expr
group = do
  modifyState (+ 1)
  symbol "("
  e <- expr
  modifyState (subtract 1)
  symbol ")"
  return e

var :: Parser Expr
var = Var <$> ident

abs' :: Parser Expr
abs' = do
  lambda
  bind <- many1 (lower <* ws)
  symbol "."
  e <- expr
  let inner = Abs (last bind) e
  return $ foldr Abs inner (init bind)

app :: Parser Expr
app = do
  a <- many1 ((group <|> abs' <|> var) <* ws)
  guard $ length a >= 2
  return $ foldl1 App a

expr :: Parser Expr
expr = group <|> abs' <|> try app <|> var

def :: Parser Stmt
def = do
  i <- ident
  symbol ":="
  Def i <$> expr

stmt :: Parser Stmt
stmt = try def <|> (Expr <$> expr)

program :: Parser [Stmt]
program = wsn *> (stmt `sepEndBy` wsn) <* eof

parse :: String -> String -> Either ParseError [Stmt]
parse = runParser program 0
