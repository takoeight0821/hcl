-- -*- mode: fundamental -*-
{
module Parser where

import Token
}

%name parse
%tokentype { Token }
%error { parseError }

%token
TINT { Token (SYMBOL "int") _ }
TVOID { Token (SYMBOL "void") _ }
TRETURN { Token (SYMBOL "return") _ }
IDENT { Token (IDENT _) _ }
LINT { Token (INTEGER _) _ }
'(' { Token (SYMBOL "(") _ }
')' { Token (SYMBOL ")") _ }
'{' { Token (SYMBOL "{") _ }
'}' { Token (SYMBOL "}") _ }
';' { Token (SYMBOL ";") _ }
',' { Token (SYMBOL ",") _ }

%%

decls : decl       {}
      | decls decl {}
decl : function_decl {}

function_decl : return_type IDENT '(' params ')' '{' stmts '}' {}

stmts :                {}
      | stmt ';'       {}
      | stmts stmt ';' {}

stmt : TRETURN exp {}

exp : LINT {}

return_type : type {}

type : TINT  {}
     | TVOID {}

params :       {}
       | param {}
       | params ',' param {}

param : type IDENT {}
      | type {}

{
parseError :: [Token] -> a
parseError [] = error "Parse error at EOF"
parseError (t:_) = error $ "Parse error:" ++ show t
}