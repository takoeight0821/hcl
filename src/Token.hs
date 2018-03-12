module Token
  ( Token(..)
  , Tag(..)
  ) where

import Text.Parsec.Pos

data Token = Token Tag SourcePos
  deriving Show

data Tag = SYMBOL String
         | IDENT String
         | INTEGER Int
  deriving Show
