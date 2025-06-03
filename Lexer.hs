module Lexer (getTokens) where

import Tokens

getTokens :: String -> [[Token]]
getTokens code = fmap createTokens (lines code)

createTokens :: String -> [Token]
createTokens [] = []
createTokens ('#':xs) = []
createTokens ('\"':xs) = 
 let (token,'\"':rest) = span (/= '\"') xs in (Token token STRING):createTokens rest
createTokens ('+':xs) = (Token "+" PLUS):createTokens xs
createTokens ('-':xs) = (Token "-" MINUS):createTokens xs
createTokens ('*':xs) = (Token "*" ASTERISK):createTokens xs
createTokens ('/':xs) = (Token "/" SLASH):createTokens xs
createTokens ('=':xs) = 
 case xs of
  '=':s -> (Token "==" EQEQ):createTokens s
  _     -> (Token "=" EQU):createTokens xs
createTokens ('>':xs) =
 case xs of
  '=':s -> (Token ">=" GRTEQ):createTokens s
  _     -> (Token ">" GRT):createTokens xs
createTokens ('<':xs) =
 case xs of
  '=':s -> (Token "<=" LESSTEQ):createTokens s
  _     -> (Token "<" LESST):createTokens xs
createTokens ('!':'=':xs) = (Token "!=" NOTEQ):createTokens xs
createTokens (' ':xs) = createTokens xs
createTokens ('\t':xs) = createTokens xs
createTokens (x:xs)
 | x `elem` ['0'..'9'] =
    let (token,rest) = span (/= ' ') xs in (Token (x:token) NUMBER):createTokens rest
 | x `elem` ['A'..'Z'] =
    let (token,rest) = span (/= ' ') xs 
        tokentype = read (x:token) :: TokenType
    in (Token (x:token) tokentype):createTokens rest
 | x `elem` ['a'..'z'] =
    let (token,rest) = span (/= ' ') xs in (Token (x:token) IDENT):createTokens rest
createTokens ('\t':xs) = createTokens xs
