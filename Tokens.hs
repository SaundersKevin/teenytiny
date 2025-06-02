module Tokens 
( TokenType (..)
, TokenData
, Token (..)
, Code
, isEqualTokenType
)
where

data TokenType = EOF 
               | NEWLINE 
               | NUMBER 
               | IDENT 
               | STRING 
               
               | LABEL 
               | GOTO 
               | PRINT 
               | INPUT 
               | LET 
               | IF | THEN | ENDIF 
               | WHILE | REPEAT | ENDWHILE 
               
               | EQU 
               | PLUS 
               | MINUS 
               | ASTERISK 
               | SLASH 
               | EQEQ 
               | NOTEQ 
               | LESST 
               | LESSTEQ 
               | GRT 
               | GRTEQ 
               deriving (Show, Read, Eq)

type TokenData = String

type Code = String

data Token = Token TokenData TokenType deriving (Show)

isEqualTokenType :: Token -> Token -> Bool
isEqualTokenType (Token _ a) (Token _ b) = a == b
