module Lib
  ( entry,
  )
where

import Parser
import Ast
import Text.ParserCombinators.Parsec.Error (ParseError)

entry :: String -> Either ParseError Program
entry = parse "(unknown)"

