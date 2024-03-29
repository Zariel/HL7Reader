module Parser.Fields
( parseDT
, parseXPN
, parseName
, parseDate
) where

import Text.ParserCombinators.Parsec

import HL7Fields
import Parser.Util
import Parser.ParserTypes

type HL7FieldParser = HL7Parser HL7Field

type HL7FieldParserEither = HL7Parser (Either HL7Field String)

getField :: HL7Parser String
getField = do
    x <- many $ noneOf "^"
    char '^'

    return x

eitherSegment :: HL7Parser (Either HL7Field String)
eitherSegment = do
    seg <- segment

    return $ Right seg

parseEitherSegment :: (HL7FieldParserEither) -> HL7FieldParserEither
parseEitherSegment f = (try $ f) <|> eitherSegment

parseDate :: HL7Parser (Either HL7Field String)
parseDate = parseEitherSegment parseDT

parseDT :: HL7FieldParserEither
parseDT = do
    date <- count 8 digit
    time <- many $ digit

    nextSep

    return $ Left $ DT date time

parseName :: HL7Parser (Either HL7Field String)
parseName = parseEitherSegment parseXPN

parseXPN :: HL7Parser (Either HL7Field String)
parseXPN = do
    foreName <- getField
    surName <- getField
    suffix <- getField
    prefix <- getField

    nextSep

    return $ Left $ XPN foreName surName suffix prefix
