module ProgramTree 
( ProgramTree (..)
, Function (..)
, Command (..)
, Parameter (..)
)
where

import Tokens

type ProgramTree = [Function]

data Function = Function Command [Parameter] deriving (Show)

type Command = TokenType

data Parameter = Ident Token
               | Number Token
               | ParamString Token
               | Compare Token
               | Func [Token]
               | Tree ProgramTree
               | Equation [Token]
               | Boolean Token
               | Expression Parameter Token Parameter
               deriving (Show)

--data Conditional = 
