import System.Directory (listDirectory)
import Lib (entry)
import Text.ParserCombinators.Parsec.Error (ParseError)
import Ast (Program, showProgram)
import Control.Arrow (ArrowChoice(left))
import System.Directory.Internal.Prelude (exitFailure)
import Data.Either (isRight)
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Data.List (intercalate)

show' :: Show a => Either a Program -> Either String String
show' (Left a) = Left (show a)
show' (Right b) = Right (showProgram b)

expect :: Bool -> String -> Either String ()
expect True s = Right ()
expect False s = Left s

showResult :: FilePath -> Either String () -> String
showResult path (Left err) = printf "File: %s. status: failed\n\t%s" path err
showResult path (Right ()) = printf "File: %s. status: pass" path

testSpecFile :: String -> Either String ()
testSpecFile contents = do
  expected <- show' $ entry contents
  roundTwo <- show' $ entry expected
  expect (roundTwo == expected) (printf "Spec file doesn't yield consistent output:\n\t  Actual: %s\n\tExpected: %s" roundTwo expected)

main :: IO ()
main = do
  specFiles <- map (printf "spec/%s") <$> listDirectory "spec/"
  specs <- mapM readFile specFiles
  let results = map testSpecFile specs
  putStrLn $ intercalate "\n\n" $ zipWith showResult specFiles results
  if all isRight results then return () else fail "Not all tests pass"
