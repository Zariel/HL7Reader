module Parser.Util
( nextSep
, segment
, firstVal
, endSeg
, segSep
, header
) where

import Text.ParserCombinators.Parsec

import Parser.ParserTypes

nextSep :: HL7Parser Char
nextSep = do
    sep <- getState
    char sep

-- Parsec combinators
segment :: HL7Parser String
segment = do
    sep <- getState
    x <- many $ noneOf [ sep ]
    nextSep

    return x

firstVal :: HL7Parser Char
firstVal = anyChar

header :: String -> HL7Parser ()
header s = do
    sep <- getState
    string s
    nextSep

    return ()

-- Consumes the rest of the string until end of segment
endSeg :: HL7Parser String
endSeg = many $ noneOf "\r"

segSep :: HL7Parser Char
segSep = char '\r'
