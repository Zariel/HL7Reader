module Parser.HL7Parser
( parseMSH
, parsePID
) where

import Text.ParserCombinators.Parsec
import Data.Maybe

import HL7Segments

-- Parsec combinators
seperator :: Char -> Parser String
seperator sep = do
    x <- many $ noneOf [ sep ]
    char sep
    return x

optionSep :: Char -> Parser (Maybe String)
optionSep sep = do
    x <- optionMaybe $ many1 $ noneOf [ sep ]
    char sep
    return x

firstVal :: Parser Char
firstVal = anyChar

endSeg :: Parser String
endSeg = do
    manyTill anyChar $ try $ string "\r"

-- API
parseMSH :: String -> Either ParseError HL7Segment
parseMSH = parse parseMSH' "(MSH)"

parseMSH' :: Parser HL7Segment
parseMSH' = do
    sep <- firstVal

    encoding <- seperator sep

    sendingApp <- optionSep sep

    sendingFac <- optionSep sep

    recvApp <- optionSep sep

    recvFac <- optionSep sep

    dateTime <- seperator sep

    security <- optionSep sep

    messageType <- seperator sep

    messageID <- seperator sep

    processID <- seperator sep

    versionID <- seperator sep

    endSeg

    return $ MSH [ sep ] encoding sendingApp sendingFac recvApp recvFac dateTime security messageType messageID processID versionID

parsePID :: String -> Maybe HL7Segment
parsePID x = case parse parsePID' "(PID)" x of
                  Left err -> Nothing
                  Right pid -> Just pid

parsePID' :: Parser HL7Segment
parsePID' = do
    setID <- optionSep sep
    patientID <- seperator sep
    patientIDList <- seperator sep
    otherID <- seperator sep
    name <- seperator sep
    maidenName <- optionSep sep
    dob <- optionSep sep
    sex <- optionSep sep
    alias <- optionSep sep
    race <- optionSep sep

    endSeg

    return $ PID setID patientID patientIDList otherID name maidenName dob sex alias race

    where
        sep = '|'
