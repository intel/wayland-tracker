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

-- make bytestring length at least specified
padBs :: Int -> BS.ByteString -> BS.ByteString
padBs neededSize bs =
    let
        extra = neededSize - BS.length bs
        padding n = C8.replicate n ' '
    in
        if extra > 0
            then BS.append bs $ padding extra
            else bs

-- split bytestring at chunkSize with another bytestring in between
-- 12345678 can become 12 34 56 78
splitBs :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
splitBs chunkSize between bs = BS.intercalate between $ split bs []
    where
        split bs acc =
            if BS.null bs
                then reverse $ acc
                else let
                        chunk = BS.take chunkSize bs
                     in
                        split ((BS.drop chunkSize) bs) (chunk:acc)

generateTS :: Clock.NominalDiffTime -> BS.ByteString
generateTS time = BS.concat $ [C8.pack "[", padBs 12 $ C8.pack (show time), C8.pack "]" ]

bSpace :: C8.ByteString
bSpace = C8.singleton ' '

bNewLine :: C8.ByteString
bNewLine = C8.singleton '\n'

toStringBinary :: BS.ByteString -> ParsedBinaryMessage -> BS.ByteString
toStringBinary ts (ParsedBinaryMessage t sender opcode size d) =
    let typeS = C8.pack $ getMessageTypeString t
        senderS = BS.concat [C8.pack "sender=", padBs 2 $ C8.pack (show sender)]
        opcodeS = BS.concat [C8.pack "op=", padBs 2 $ C8.pack (show opcode)]
        sizeS = BS.concat [C8.pack "size=", padBs 2 $ C8.pack (show size)]
        dataS = splitBs 8 bSpace $ B16.encode d -- split between 8 hex chars
    in
        BS.concat [ts, bSpace, typeS, bSpace, senderS, bSpace, opcodeS, bSpace, sizeS, bSpace, bSpace, dataS, bNewLine]

getMessageTypeString :: MessageType -> String
getMessageTypeString Event = "Event  "
getMessageTypeString Request = "Request"

writeBinaryLog :: Logger -> Clock.NominalDiffTime -> ParsedBinaryMessage -> IO ()
writeBinaryLog (Logger lh _) ts msg = do
    let stamp = generateTS ts
    BS.hPut lh $ toStringBinary stamp msg

writeLog :: Logger -> Clock.NominalDiffTime -> ParsedMessage -> IO ()
writeLog logger ts msg = return ()