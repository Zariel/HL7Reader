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

    encoding <- segment
    sendingApp <- optionMaybe $ segment
    sendingFac <- optionMaybe $ segment
    recvApp <- optionMaybe $ segment
    recvFac <- optionMaybe $ segment
    dateTime <- parseDate
    security <- optionMaybe $ segment
    messageType <- segment
    messageID <- segment
    processID <- segment
    versionID <- segment

    endSeg

    return $ MSH sep encoding sendingApp sendingFac recvApp recvFac dateTime security messageType messageID processID versionID

parsePID :: HL7SegParser
parsePID = do
    header "PID"

    setID <- optionMaybe $ segment
    patientID <- segment
    patientIDList <- segment
    otherID <- segment
    name <- parseName
    maidenName <- optionMaybe $ segment
    dob <- optionMaybe $ segment
    sex <- optionMaybe $ segment
    alias <- optionMaybe $ segment
    race <- optionMaybe $ segment

    many $ noneOf "\r"

    return $ Just $ PID setID patientID patientIDList otherID name maidenName dob sex alias race

parseOBR :: HL7SegParser
parseOBR = do
    header "OBR"

    setID <- optionMaybe $ segment
    placer <- segment
    filler <- segment
    serviceID <- segment
    priority <- segment
    requestDate <- parseDate
    obsDate <- parseDate
    obsEndDate <- parseDate

    rest <- many $ noneOf "\r"

    return $ Just $ OBR setID placer filler serviceID priority requestDate obsDate obsEndDate rest

parseOBX :: HL7SegParser
parseOBX = do
    header "OBX"

    many $ noneOf "\r"

    return $ Just OBX
