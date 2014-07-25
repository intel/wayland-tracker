module Log (
    writeLog,
    writeBinaryLog)

where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
-- import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Char8 as C8
import qualified Data.Aeson as A
import qualified Data.Time.Clock as Clock

import Types

-- TODO: use ByteString for all string manipulation purposes

generateTS :: Clock.NominalDiffTime -> String
generateTS time = "[" ++ (show time) ++ "]"

bSpace :: C8.ByteString
bSpace = C8.singleton $ head " "

bNewLine :: C8.ByteString
bNewLine = C8.singleton $ head "\n"

toStringBinary :: String -> ParsedBinaryMessage -> BS.ByteString
toStringBinary ts (ParsedBinaryMessage t sender opcode size d) =
    let typeS = C8.pack $ getMessageTypeString t
        senderS = C8.pack $ "sender=" ++ show sender
        opcodeS = C8.pack $ "opcode=" ++ show opcode
        sizeS = C8.pack $ "size=" ++ show size
        dataS = B16.encode d -- TODO: split in chunks
    in
        BS.concat [C8.pack ts, bSpace, typeS, bSpace, senderS, bSpace, opcodeS, bSpace, sizeS, bSpace, dataS, bNewLine]

getMessageTypeString :: MessageType -> String
getMessageTypeString Event = "Event  "
getMessageTypeString Request = "Request"

writeBinaryLog :: Logger -> Clock.NominalDiffTime -> ParsedBinaryMessage -> IO ()
writeBinaryLog (Logger lh _) ts msg = do
    let stamp = generateTS ts
    BS.hPut lh $ toStringBinary stamp msg

writeLog :: Logger -> Clock.NominalDiffTime -> ParsedMessage -> IO ()
writeLog logger ts msg = return ()