import System.IO
import System.Environment
import qualified Control.Exception as E

import HL7Message
import HL7Segments

main :: IO ()
main = E.catch main' handler

main' :: IO ()
main' = do
    args <- getArgs
    content <- readFile $ handleArgs args
    pprint $ readHL7 content
    where
        handleArgs [] = ""
        handleArgs (x:_) = x

handler :: IOError -> IO ()
handler = ioError

readHL7 :: String -> [ HL7Segment ]
readHL7 = fromString

pprint :: [ HL7Segment ] -> IO ()
pprint [] = return ()
pprint (x:xs) = do
    putStrLn $ show x
    pprint xs
