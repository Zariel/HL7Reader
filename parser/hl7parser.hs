module Parser.HL7Parser
( parseMessage
, parseSegments
, parseSegment
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

-- Consumes the rest of the string until end of segment
endSeg :: Parser String
endSeg = manyTill anyChar $ try $ string "\r"

consume s = manyTill anyChar $ try $ char s

segSep :: Parser Char
segSep = char '\r'

-- API
parseMessage :: Parser [ HL7Segment ]
parseMessage = do
    msh <- parseMSH
    let sep = getSeperator msh
    segments <- parseSegments sep

    return (msh : segments)

parseSegments :: Char -> Parser [ HL7Segment ]
parseSegments sep = fmap catMaybes $ (parseSegment sep) `sepEndBy` segSep

parseSegment :: Char -> Parser (Maybe HL7Segment)
parseSegment s = (try $ parsePID s) <|> (try $ parseOBR s) <|> unknown
    where unknown = do
            many $ noneOf "\r"
            return Nothing

parseMSH = do
    msh <- string "MSH"
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

    return $ MSH sep encoding sendingApp sendingFac recvApp recvFac dateTime security messageType messageID processID versionID

parsePID :: Char -> Parser (Maybe HL7Segment)
parsePID sep = do
    string "PID"
    char sep

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

    many $ noneOf "\r"

    return $ Just $ PID setID patientID patientIDList otherID name maidenName dob sex alias race

parseOBR :: Char -> Parser (Maybe HL7Segment)
parseOBR sep = do
    string "OBR"
    char sep

    setID <- optionSep sep

    many $ noneOf "\r"

    return $ Just $ OBR setID
