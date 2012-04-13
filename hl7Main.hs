import System.IO
import System.IO.Error
import System.Environment

import HL7Message

main :: IO ()
main = catch main' handler

main' :: IO ()
main' = do
    args <- getArgs
    content <- readFile $ handleArgs args
    putStr $ readHl7 content
    where
        handleArgs [] = ""
        handleArgs (x:_) = x

handler :: IOError -> IO ()
handler = ioError

readHl7 :: String -> String
readHl7 msg = show $ HL7Message.fromString msg
