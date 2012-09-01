module Parser.HL7Parser
( parseMessage
, HL7Parser
) where

import HL7Segments

import Parser.Segments
import Parser.ParserTypes

-- API
parseMessage :: HL7Parser [ HL7Segment ]
parseMessage = do
    msh <- parseMSH
    segments <- parseSegments

    return (msh : segments)
