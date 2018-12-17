module Main where

import Prelude
import System.Environment
import Language.C.Parser
import Language.C.Data.Position
import Language.C.Data.InputStream

main :: IO ()
main = do
  (file:_) <- getArgs
  src <- readInputStream file
  print $ parseC src (initPos file)
