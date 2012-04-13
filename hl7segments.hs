module HL7Segments
( HL7Segment(..)
) where

import Data.Maybe

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
                , getDateTime       :: String
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
                , getPatientName    :: String
                , getMaidenName     :: Maybe String
                , getDOB            :: Maybe String
                , getSex            :: Maybe String
                , getPatientAlias   :: Maybe String
                , getRace           :: Maybe String
                }
               | OBR
                { getSetID          :: Maybe String
                }
              deriving (Show, Eq, Ord)
