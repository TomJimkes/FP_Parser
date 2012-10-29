module Main where

import TooSimpleParseLib

func :: Parser Char String
func = (many $ pSatisfy ( /=')' ))

func1 :: Parser Char String
func1 = (many $ pSatisfy ( /='}' ))
{-
pMany :: f -> [Char] -> (f -> [Char])
pMany f lis@(x:xs) | many pSatisfy (/= ".") = pMany f list
				   | f list-}



myParse :: Parser Char (String,String)
myParse = (\_ ex bdy -> (ex,bdy) ) <$> pToken "if("
								 <*> func
								 <* pToken "){"
								 <*> func1
								 <* pSym '}'

