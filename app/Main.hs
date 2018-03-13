module Main where

import Prelude hiding (lex)
import System.Environment
import Lexer

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [_] -> pure ()
    _ -> error $ "usage: " ++ prog ++ " filename"
  src <- readFile (head args)
  case lex (head args) src of
    (Left err) -> error $ show err
    (Right toks) -> print toks
