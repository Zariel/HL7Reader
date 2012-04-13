module HL7Message
( HL7Message(..)
, fromString
) where

import Data.List.Split
import Data.Maybe

import HL7Segments
import Parser.HL7Parser

data HL7Message =
                ACK String
              | NAK
              | ORU
                { getMSH :: HL7Segment
                , getPID :: Maybe HL7Segment
                }
              deriving (Show, Eq, Ord)

fromString :: String -> HL7Message
fromString s = parseSegments $ splitAt 3 s

parseSegments :: (String, String) -> HL7Message
parseSegments ([], _) = NAK
parseSegments ("MSH", xs) = case parseMSH xs of
                        Left err -> NAK
                        Right msh -> parseSegments' msh xs
parseSegments (_, _) = NAK

parseSegments' :: HL7Segment -> String -> HL7Message
parseSegments' (msh @ MSH { getMessageType = "ORU^R01" }) rest = parseORU msh (splitOn "\r" rest)
parseSegments' _ _= NAK

parseORU :: HL7Segment -> String -> HL7Message
parseORU (msh @ MSH {}) rest = ORU msh pid
    where pid = parsePID rest
parseORU _ _ = NAK
