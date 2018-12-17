module Main where

import           Prelude
import           System.Environment
import Parser

main :: IO ()
main = do
  src <- getContents
  print <$> parse src
