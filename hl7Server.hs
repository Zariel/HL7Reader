module HL7Server
( hl7Server
) where

import System.IO
import Network
import Control.Concurrent
import Data.List

import HL7Segments
import HL7Message

port = 555

hl7Server = withSocketsDo $ do
    print ("Listening on port " ++  show port)
    sock <- listenOn $ PortNumber port
    loop sock

loop :: Socket -> IO b
loop sock = do
    (h, _, _) <- accept sock
    forkIO $ handleMessage h
    loop sock

handleMessage :: Handle -> IO ()
handleMessage h = do
    input <- hGetLine h
    print input
    hPutStr h (handleGet "GET" (HL7Message.fromString input))
    hFlush h
    hClose h

handleGet :: String -> HL7Message -> String
handleGet "GET" h = show m200
handleGet _ _ = show m404

m200 = ACK "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
m404 = NAK "HTTP/1.0 404\r\nContent-Length: 5\r\n\r\nNope!\r\n"
