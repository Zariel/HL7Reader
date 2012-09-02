module Parser.Util
( seperator
, firstVal
, endSeg
, segSep
, header
) where

import Text.ParserCombinators.Parsec

import Parser.ParserTypes

-- Parsec combinators
seperator :: HL7Parser String
seperator = do
    sep <- getState
    x <- many $ noneOf [ sep ]
    char sep

    return x

firstVal :: HL7Parser Char
firstVal = anyChar

header :: String -> HL7Parser ()
header s = do
    sep <- getState
    string s
    char sep

    return ()

-- Consumes the rest of the string until end of segment
endSeg :: HL7Parser String
endSeg = many $ noneOf "\r"

segSep :: HL7Parser Char
segSep = char '\r'
