module Parser.Segments
( parseSegments
, parseMSH
, parsePID
, parseOBR
) where

import Data.Maybe
import Text.ParserCombinators.Parsec

import HL7Segments
import Parser.Util
import Parser.Fields
import Parser.ParserTypes

parseSegments :: HL7Parser [ HL7Segment ]
parseSegments = fmap catMaybes $ (parseSegment) `sepEndBy` segSep

type HL7SegParser = HL7Parser (Maybe HL7Segment)

(<+>) :: HL7SegParser -> HL7SegParser -> HL7SegParser
f <+> g = (try $ f) <|> (try $ g)

parseSegment :: HL7SegParser
parseSegment = parsePID <+> parseOBR <+> parseOBX <+> unknown
    where unknown = do
            many $ noneOf "\r"
            return Nothing

parseMSH :: CharParser Char (HL7Segment)
parseMSH = do
    msh <- string "MSH"
    sep <- firstVal

    updateState (return sep)

    encoding <- seperator
    sendingApp <- optionSep
    sendingFac <- optionSep
    recvApp <- optionSep
    recvFac <- optionSep
    dateTime <- parseDate
    security <- optionSep
    messageType <- seperator
    messageID <- seperator
    processID <- seperator
    versionID <- seperator

    endSeg

    return $ MSH sep encoding sendingApp sendingFac recvApp recvFac dateTime security messageType messageID processID versionID

parsePID :: HL7SegParser
parsePID = do
    header "PID"

    setID <- optionSep
    patientID <- seperator
    patientIDList <- seperator
    otherID <- seperator
    name <- parseName
    maidenName <- optionSep
    dob <- optionSep
    sex <- optionSep
    alias <- optionSep
    race <- optionSep

    many $ noneOf "\r"

    return $ Just $ PID setID patientID patientIDList otherID name maidenName dob sex alias race

parseOBR :: HL7SegParser
parseOBR = do
    header "OBR"

    setID <- optionSep
    placer <- seperator
    filler <- seperator

    many $ noneOf "\r"

    return $ Just $ OBR setID placer filler

parseOBX :: HL7SegParser
parseOBX = do
    header "OBX"

    many $ noneOf "\r"

    return $ Just OBX
