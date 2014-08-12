{-
Copyright © 2014 Intel Corporation

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation, and
that the name of the copyright holders not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  The copyright holders make no representations
about the suitability of this software for any purpose.  It is provided "as
is" without express or implied warranty.

THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
OF THIS SOFTWARE.
-}

{-# LANGUAGE OverloadedStrings #-}

module Log (
    writeLog,
    writeBinaryLog)

where

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Char8    as C8
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Time.Clock          as Clock
import qualified System.IO                as IO

import           Types

data StampedMessage = StampedMessage String ParsedMessage deriving (Eq, Show)

instance A.ToJSON StampedMessage where
    toJSON (StampedMessage time msg) = A.object
        [ "timestamp" A..= time, "message" A..= msg ]


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
-- example: 123456789 becomes 12 34 56 78 9 if padding is two
splitBs :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
splitBs chunkSize between bstr = BS.intercalate between $ split bstr []
    where
        split bs acc =
            if BS.null bs
                then reverse $ acc
                else let
                        chunk = BS.take chunkSize bs
                     in
                        split ((BS.drop chunkSize) bs) (chunk:acc)


generateTS :: Clock.NominalDiffTime -> BS.ByteString
generateTS time = BS.concat [C8.pack "[", padBs 12 $ C8.pack (show time), C8.pack "]" ]


bSpace :: C8.ByteString
bSpace = C8.singleton ' '


bNewLine :: C8.ByteString
bNewLine = C8.singleton '\n'


toStringBinary :: BS.ByteString -> ParsedBinaryMessage -> BS.ByteString
toStringBinary ts (ParsedBinaryMessage t sender opcode size d) =
    let typeS = padBs 7 $ getMessageTypeString t
        senderS = BS.concat [C8.pack "sender=", padBs 2 $ C8.pack (show sender)]
        opcodeS = BS.concat [C8.pack "op=", padBs 2 $ C8.pack (show opcode)]
        sizeS = BS.concat [C8.pack "size=", padBs 2 $ C8.pack (show size)]
        dataS = splitBs 8 bSpace $ B16.encode d -- split between 8 hex chars
    in
        BS.concat [ts, bSpace, typeS, bSpace, senderS, bSpace, opcodeS, bSpace, sizeS, bSpace, bSpace, dataS, bNewLine]


getMessageTypeString :: MessageType -> BS.ByteString
getMessageTypeString Event = C8.pack "Event"
getMessageTypeString Request = C8.pack "Request"


writeBinaryLog :: Logger -> Clock.NominalDiffTime -> ParsedBinaryMessage -> IO ()
writeBinaryLog (Logger lh _) ts msg = do
    let stamp = generateTS ts
    BS.hPut lh $ toStringBinary stamp msg
    IO.hFlush lh


toSimple :: Clock.NominalDiffTime -> ParsedMessage -> BS.ByteString
toSimple ts (UnknownMessage t) = let
    stamp = generateTS ts
    arrow = case t of
        Request -> C8.pack " ->"
        Event -> C8.pack "<- "
    in
        BS.concat [stamp, bSpace, arrow, bSpace, C8.pack "Unknown message"]

toSimple ts (Message t n i o args) = let
    stamp = generateTS ts
    arrow = case t of
        Request -> C8.pack " ->"
        Event -> C8.pack "<- "
    simpleArgs as = BS.intercalate (C8.pack ", ") (map argToString as)
    argToString (MArgument _ a) = case a of
        MInt v -> C8.pack $ show v
        MUInt v -> C8.pack $ show v
        MString v -> BS.concat [C8.singleton '"', C8.pack v, C8.singleton '"']
        MFixed s fp sp -> BS.concat $ [C8.singleton '-' | s] ++
                [C8.pack $ show fp, C8.singleton '.', C8.pack $ show sp]
        MArray bs -> BS.concat [C8.singleton '[', B16.encode bs, C8.singleton ']']
        MFd -> C8.pack "fd"
        MNewId object interface -> let
            i = if null interface then C8.pack "[unknown]" else C8.pack interface
            in
                BS.concat [C8.pack "new id ", i, C8.singleton '@', C8.pack $ show object]
        MObject v -> BS.concat [C8.pack "object ", C8.pack $ show v]

    in
        BS.concat [stamp, bSpace, arrow, bSpace, C8.pack i, C8.singleton '@',
                   C8.pack $ show o, C8.singleton '.', C8.pack n, C8.singleton '(',
                   simpleArgs args, C8.singleton ')']


writeLog :: Logger -> Clock.NominalDiffTime -> ParsedMessage -> IO ()
writeLog (Logger lh lt) ts msg = do
    -- let stamp = generateTS ts
    let smsg = StampedMessage (show ts) msg
    case lt of
        Json -> do
            BSL.hPut lh $ A.encode smsg
            BS.hPut lh bNewLine
            IO.hFlush lh
        JsonPretty -> do
            BSL.hPut lh $ AP.encodePretty' conf smsg
            BS.hPut lh bNewLine
            IO.hFlush lh
        Simple -> do
            BS.hPut lh $ toSimple ts msg
            BS.hPut lh bNewLine
            IO.hFlush lh
    where
        conf = AP.Config 4 ordering
        ordering = AP.keyOrder ["type", "name", "object", "interface", "arguments", "value", "message", "timestamp"]
