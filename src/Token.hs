module Token
  ( Token(..)
  , Tag(..)
  ) where

import Text.Parsec.Pos

data Token = Token { _tag :: Tag
                   , _pos :: SourcePos
                   }
  deriving Show

data Tag = SYMBOL { _sym :: String }
         | IDENT { _id :: String }
         | INTEGER { _int :: Int }
  deriving Show