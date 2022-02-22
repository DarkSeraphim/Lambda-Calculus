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
eol = endOfLine $> () <|> eof

comment :: Parser String
comment = char ';' *> manyTill anyChar (lookAhead eol)

maybeEol :: Parser ()
maybeEol = (() <$ flip when (optional (endOfLine *> ws))) . (>= 1) <$> getState

ws :: Parser ()
ws = many (oneOf space') *> optional comment *> maybeEol

wsn :: Parser ()
wsn = ws *> optional (endOfLine *> wsn)

lexeme :: Parser a -> Parser a
lexeme a = a <* ws

symbol :: String -> Parser String
symbol s = lexeme $ string s

ident :: Parser String
ident = lexeme $ many1 (noneOf (space' ++ "\n;()"))

lambda :: Parser String
lambda = symbol "λ" <|> symbol "\\"

incDepth :: Parser ()
incDepth = modifyState (+ 1)

decDepth :: Parser()
decDepth = modifyState (subtract 1)

group :: Parser Expr
group = incDepth *> symbol "(" *> expr <* symbol ")" <* decDepth

var :: Parser Expr
var = Var <$> ident

abs' :: Parser Expr
abs' = flip (foldr Abs) <$> (lambda *> many1 (lexeme lower) <* symbol ".") <*> expr

expr :: Parser Expr
expr = foldl1 App <$> many1 (lexeme $ group <|> abs' <|> var)

def :: Parser Stmt
def = Def <$> (ident <* symbol ":=") <*> expr

stmt :: Parser Stmt
stmt = try def <|> Expr <$> expr

program :: Parser Program
program = wsn *> (stmt `sepEndBy` wsn) <* eof

parse :: String -> String -> Either ParseError Program
parse = runParser program 0
