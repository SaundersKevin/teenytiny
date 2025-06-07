module Set where

import Tokens
import qualified Data.Map as Map

type Definitions = [(Token,Token)]

parseSet :: [[Token]] -> [[Token]]
parseSet tokens = 
 let (definitions,withoutSetTokens) = checkSet tokens
 in map (insertSet definitions) withoutSetTokens
     
checkSet :: [[Token]] -> (Definitions,[[Token]])
checkSet (((Token "SET" SET):var:val:_):tokens) = 
 let (definitions,tokenList) = checkSet tokens
 in  ((var,val):definitions,tokenList)
checkSet (_:tokens) = ([],tokens)

insertSet :: Definitions -> [Token] -> [Token]
insertSet _ [] = []
insertSet defs (token:rest) = if tokenIsDefined defs token then (findDef token defs):(insertSet defs rest) else token:insertSet defs rest


-- Helper Functions --

testing compare acc test = if getData test == getData compare then True else acc
tokenIsDefined list testedToken = foldl (\acc -> testing (testedToken) acc . fst) False list

findDef :: (Eq k) => k -> [(k,v)] -> v  
findDef token defs = snd . head . filter (\(k,v) -> token == k) $ defs
