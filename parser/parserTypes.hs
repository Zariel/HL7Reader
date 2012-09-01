module Parser.ParserTypes
( HL7Parser
) where

import Text.ParserCombinators.Parsec

type HL7Parser a = CharParser Char (a)
