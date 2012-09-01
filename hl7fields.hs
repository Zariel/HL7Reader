module HL7Fields
( HL7Field(..)
, HL7FieldString
) where

type HL7FieldString = Either HL7Field String

data HL7Field =
                DT
                 { getDate :: String
                 , getTime :: String
                 }
                | XPN
                 { getFamilyName :: String
                 , getForeName :: String
                 , getSuffix :: String
                 , getPrefix :: String
                 }
                deriving (Show, Eq, Ord)
