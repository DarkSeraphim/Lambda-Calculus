module Lib
  ( entry,
  )
where

import Parser
import System.Environment

entry :: IO ()
entry = do
  args <- getArgs
  print $ parse "(unknown)" (head args)
