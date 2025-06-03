module Parser (toProgramTree) where

import Tokens

import ProgramTree

toProgramTree :: ProgramTree -> [[Token]] -> ProgramTree
toProgramTree tree [] = tree
toProgramTree tree ([]:tokenList) = toProgramTree tree tokenList
toProgramTree tree ((headToken:params):tokenList) =
 case headToken of
  (Token _ PRINT) -> let (function, remainingTokens) = printFunc (params:tokenList) in toProgramTree (tree ++ function) remainingTokens
  (Token _ LET)   -> let (function, remainingTokens) = letFunc (params:tokenList) in toProgramTree (tree ++ function) remainingTokens
  (Token _ WHILE) -> let (function, remainingTokens) = whileFunc (params:tokenList) in toProgramTree (tree ++ function) remainingTokens
  (Token _ LABEL) -> let (function, remainingTokens) = labelFunc (params:tokenList) in toProgramTree (tree ++ function) remainingTokens
  (Token _ GOTO)  -> let (function, remainingTokens) = gotoFunc (params:tokenList) in toProgramTree (tree ++ function) remainingTokens
  (Token _ INPUT) -> let (function, remainingTokens) = inputFunc (params:tokenList) in toProgramTree (tree ++ function) remainingTokens
  (Token _ IF)    -> let (function, remainingTokens) = ifFunc (params:tokenList) in toProgramTree (tree ++ function) remainingTokens
-- Function -- Token is the first of the line (the command token)

-- The returned function will take so many lines of the list to parse,
-- parse them, and append the Function to the ProgramTree and return the rest of the
-- tokens

inputFunc :: [[Token]] -> ([Function], [[Token]])
inputFunc ((ident:_):remainingTokens) = ([Function INPUT [Ident ident]], remainingTokens)

labelFunc :: [[Token]] -> ([Function], [[Token]])
labelFunc ((label:_):remainingTokens) = ([Function LABEL [Ident label]], remainingTokens)

gotoFunc :: [[Token]] -> ([Function], [[Token]])
gotoFunc ((goto:_):remainingTokens) = ([Function GOTO [Ident goto]], remainingTokens)

-- For now I am treating the equation as a NUMBER or IDENT
letFunc :: [[Token]] -> ([Function], [[Token]])
letFunc ((ident:(_:equation)):remainingTokens) = ([Function LET [Ident ident, Equation equation]], remainingTokens)

------------
-- Helper Functions for control loops
------------

compareTokens = [Token "EQEQ" EQEQ, Token "NOTEQ" NOTEQ, Token "LESST" LESST, Token "LESSTEQ" LESSTEQ, Token "GRT" GRT, Token "GRTEQ" GRTEQ]
testing comp acc test = if isEqualTokenType test comp then True else acc
isCompare test = foldl (testing test) False compareTokens
split = span (\param -> not $ isCompare param)

-----------

whileFunc :: [[Token]] -> ([Function], [[Token]])
whileFunc (params:tokenList) =
 let (equation1, compare:equation2) = split params

     (whileLoopTokens, endWhile:remainingTokens) = span (\token -> if token == [] then True else ( not . isEqualTokenType (Token "ENDWHILE" ENDWHILE) . head $ token)) tokenList
     whileProgramTree = toProgramTree [] whileLoopTokens
 in  if length params == 1 then ([Function WHILE [Boolean (head params), Tree whileProgramTree]], remainingTokens) else ([Function WHILE [Equation equation1, Compare compare, Equation equation2, Tree whileProgramTree]],remainingTokens)

ifFunc :: [[Token]] -> ([Function], [[Token]])
ifFunc (params:tokenList) =
 let (equation1, compare:equation2) = split params

     (ifLoopTokens, endIf:remainingTokens) = span (not . isEqualTokenType (Token "ENDIF" ENDIF) . head) tokenList
     ifProgramTree = toProgramTree [] ifLoopTokens
 in  if length params == 1 then ([Function IF [Boolean (head params), Tree ifProgramTree]], remainingTokens) else ([Function IF [Equation equation1, Compare compare, Equation equation2, Tree ifProgramTree]], remainingTokens)
------------

printFunc :: [[Token]] -> ([Function], [[Token]])
printFunc (((Token tokenData STRING):_):tokenList) = ([Function PRINT [ParamString (Token tokenData STRING)]], tokenList)
printFunc (((Token tokenData IDENT):_):tokenList) = ([Function PRINT [Ident (Token tokenData IDENT)]], tokenList)
