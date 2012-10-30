module Main where

import Data.Char
import TooSimpleParseLib

pStatement :: String
pStatement = show pCountry

pCountry :: Parser Char String
pCountry = (\_ c1 c2 -> c1 ++ c2) <$> overig
								 <* pToken " "
								 <*> pCapital
								 <*> pRest

pCapital :: Parser Char String
pCapital = (many $ pSatisfy (`elem` ['A'..'Z']))

pRest :: Parser Char String
pRest = (many $ pSatisfy ( /=' ' ))

overig :: Parser Char String
overig = (many $ pSatisfy (`elem` ['a'..'z']))

keywords :: [String]
keywords = ["borders", "border"]

pKeywords :: String -> String
pKeywords s (x:xs) = if s `elem` keyword then s else pKeywords s xs
pKeywords s [] = _

--Testing
myParse :: Parser Char (String,String)
myParse = (\_ ex bdy -> (ex,bdy) ) <$> pToken "if("
								 <*> func
								 <* pToken "){"
								 <*> func1
								 <* pSym '}'

func :: Parser Char String
func = (many $ pSatisfy ( /=')' ))

func1 :: Parser Char String
func1 = (many $ pSatisfy ( /='}' ))

{-}
pMany :: Parser a a -> Parser a a
pMany p = -}