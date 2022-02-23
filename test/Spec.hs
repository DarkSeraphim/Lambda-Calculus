import System.Directory (listDirectory)
import Lib (entry)
import Text.ParserCombinators.Parsec.Error (ParseError)
import Ast (Program, showProgram, reprProgram)
import Control.Arrow (ArrowChoice(left))
import System.Directory.Internal.Prelude (exitFailure)
import Data.Either (isRight)
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Data.List (intercalate, isSuffixOf)
import Data.Bifunctor (Bifunctor)
import qualified Data.Bifunctor as Bifunctor

show' :: Show a => Either a Program -> Either String String
show' (Left a) = Left (show a)
show' (Right b) = Right (showProgram b)

expect :: Bool -> String -> Either String ()
expect True s = Right ()
expect False s = Left s

showResult :: FilePath -> Either String () -> String
showResult path (Left err) = printf "File: %s. status: failed\n\t%s" path err
showResult path (Right ()) = printf "File: %s. status: pass" path

testSpecFile :: Bool -> String -> Either String ()
testSpecFile False contents = do
  let result = entry contents
  case result of
    Right program -> Left (printf "Spec file should fail, but completed with: %s" (reprProgram program))
    Left err -> Right ()

testSpecFile True contents = do
  expected <- show' $ entry contents
  roundTwo <- show' $ entry expected
  let err = printf "Spec file doesn't yield consistent output:\n\t  Actual: %s\n\tExpected: %s" roundTwo expected
  expect (roundTwo == expected) err

main :: IO ()
main = do
  specFiles <- map (printf "spec/%s") <$> listDirectory "spec/"
  specs <- mapM readFile specFiles
  let results = zipWith (curry (uncurry testSpecFile . Bifunctor.first (isSuffixOf ".fails"))) specFiles specs
  putStrLn $ intercalate "\n\n" $ zipWith showResult specFiles results
  if all isRight results then return () else fail "Not all tests pass"
