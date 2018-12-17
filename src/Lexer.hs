module Lexer (lex) where

import Prelude hiding (lex)
import Token
import Text.Parsec.Pos
import Text.Parsec hiding (token)
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok

lexer = Tok.makeTokenParser $
  emptyDef { Tok.commentLine = "//"
           , Tok.commentStart = "/*"
           , Tok.commentEnd = "*/"
           , Tok.nestedComments = False
           , Tok.identStart = letter <|> char '_'
           , Tok.identLetter = alphaNum <|> char '_'
           , Tok.reservedOpNames = reservedOpNames
           , Tok.reservedNames = reservedNames
           }

reservedOpNames = [ ".", "->", "++", "--", "&", "*", "+"
                  , "-", "~", "!", "/", "%", "<<", ">>"
                  , "<", ">", "<=", ">=", "==", "!="
                  , "^", "|", "&&", "||", "?", ":", ";"
                  , "...", "=", "*=", "/=", "%=", "+="
                  , "-=", "<<=", ">>=", "&=", "^=", "|="
                  , ",", "#", "##", "<:", ":>", "<%"
                  , "%>", "%:", "%:%:", "(", ")", "[","]"
                  , "{", "}"
                  ]

reservedNames = [ "auto", "break", "case", "char"
                , "const", "continue", "default", "do"
                , "double", "else", "enum", "extern"
                , "float", "for", "goto", "if", "inline"
                , "int", "long", "register", "restrict"
                , "return", "short", "signed", "sizeof"
                , "static", "struct", "switch", "typedef"
                , "union", "unsigned", "void", "volatile"
                , "while", "_Alignas", "_Alignof", "_Atomic"
                , "_Bool", "_Complex", "_Generic"
                , "_Imaginary", "_Noreturn", "_Static_assert"
                , "_Thread_local"
                ]

symbol sym = do
  pos <- getPosition
  Tok.reserved lexer sym
  pure (Token (SYMBOL sym) pos)

op sym = do
  pos <- getPosition
  Tok.reservedOp lexer sym
  pure (Token (SYMBOL sym) pos)

ident = do
  pos <- getPosition
  str <- Tok.identifier lexer
  pure (Token (IDENT str) pos)

int = do
  pos <- getPosition
  i <- Tok.natural lexer
  pure (Token (INTEGER (fromInteger i)) pos)

token =
  foldl1 (<|>) (map symbol reservedNames)
  <|> foldl1 (<|>) (map op reservedOpNames)
  <|> ident <|> int


lex :: SourceName -> String -> Either ParseError [Token]
lex = parse (whiteSpace >> many token >>= \toks -> eof >> pure toks)
  where whiteSpace = Tok.whiteSpace lexer
