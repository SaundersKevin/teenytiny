module Tokens 
( TokenType (..)
, TokenData
, Token (..)
, Code
, isEqualTokenType
, Variables
, getData
)
where

-- Deriving Show for debugging purposes --

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
               | POP
               | PUSH
               | PEEK
               | SET
               
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

type Variables = [String]

-- Eq needed for WHILE and IF
data Token = Token TokenData TokenType deriving (Show, Eq)


-- Helper Functions --
isEqualTokenType :: Token -> Token -> Bool
isEqualTokenType (Token _ a) (Token _ b) = a == b

getData :: Token -> String
getData (Token dat _) = dat
