module HL7Segments
( HL7Segment(..)
) where

import Data.Maybe
import HL7Fields

data HL7Segment =
                MSA String
              | QRD String
              | MSH
                { getSeperator      :: Char
                , getEncoding       :: String
                , getSendingApp     :: Maybe String
                , getSendingFac     :: Maybe String
                , getRecvApp        :: Maybe String
                , getRecvFac        :: Maybe String
                , getDateTime       :: HL7FieldString
                , getSecurity       :: Maybe String
                , getMessageType    :: String
                , getMessageID      :: String
                , getProcessingID   :: String
                , getVersionID      :: String
                }
               | PID
                { getSetID          :: Maybe String
                , getPatientID      :: String
                , getPatientIDList  :: String
                , getOtherPatientID :: String
                , getPatientName    :: HL7FieldString
                , getMaidenName     :: Maybe String
                , getDOB            :: Maybe String
                , getSex            :: Maybe String
                , getPatientAlias   :: Maybe String
                , getRace           :: Maybe String
                }
               | OBR
                { getSetID          :: Maybe String
                , getPlacerNumber   :: String
                , getFillerNumber   :: String
                }
               | OBX
              deriving (Show, Eq, Ord)
