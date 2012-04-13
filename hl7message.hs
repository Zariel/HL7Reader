module HL7Message
( HL7Message(..)
, fromString
) where

import Text.ParserCombinators.Parsec
import Data.List.Split
import Data.Maybe

import HL7Segments
import Parser.HL7Parser

import Debug.Trace(trace)

data HL7Message =
                ACK String
              | NAK
              | ORU
                { getMSH :: HL7Segment
                , getPID :: Maybe HL7Segment
                }
              deriving (Show, Eq, Ord)

fromString :: String -> HL7Message
fromString s = case parse parseMessage "(hl7)" s of
                    Left err -> NAK
                    Right msg -> buildMsg msg

parseSegments' :: HL7Segment -> String -> HL7Message
parseSegments' (msh @ MSH { getMessageType = "ORU^R01" }) rest = ORU msh Nothing
parseSegments' _ _= NAK

buildMsg :: [ HL7Segment ] -> HL7Message
buildMsg a@(x:xs) = trace (show a ++ "\n") ORU x Nothing
