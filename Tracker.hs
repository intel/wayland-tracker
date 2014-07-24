module Tracker where

import qualified Data.Word as W
-- import qualified Foreign.C.Types as C
-- import qualified Data.Binary.Strict.Get as BG
import qualified Control.Monad as M
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BSL
import qualified System.IO.Error as Err
import qualified System.Environment as E
import qualified System.Exit as Exit
-- import qualified System.Posix.Env as PE
import qualified System.Posix.User as PU
import qualified System.Posix.IO as PI
import qualified System.Posix.Process as Process
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as PT
import qualified Network.Socket as Socket
-- import qualified Network.Socket.ByteString as BSocket
-- import qualified System.IO as SIO
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
-- import qualified Control.Concurrent.STM.TChan as T
import qualified Control.Monad.Error as ET
import qualified Numeric as N
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as DM
import qualified Data.IntMap as IM
import qualified Data.ByteString.UTF8 as U
import qualified Data.List as List
import qualified Data.Attoparsec.ByteString as A
-- import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Attoparsec.Binary as AB

-- import Control.Applicative

import ParseWaylandXML
import Wayland

-- import Debug.Trace

-- messages contain header and a list of data / fd blocks

data WHeader = WHeader { object :: W.Word32, size :: W.Word16, opcode :: W.Word16 } deriving (Eq, Show)

data MArgumentValue = MInt Int
                    | MUInt Int
                    | MString String
                    | MFixed Bool Int Int
                    | MArray BS.ByteString
                    | MFd
                    | MNewId Int String
                    | MObject Int
                           deriving (Eq, Show)

data MArgument = MArgument {
                     argName :: String,
                     argValue :: MArgumentValue
                 } deriving (Eq, Show)
{-
data WMessageBlock = WMessageBlock {
    start :: Int,
    dataLength :: Int,
    blockType :: WArgumentType,
    dataFd :: Int
} deriving (Eq, Show)
-}

data MessageType = Request | Event deriving (Eq, Show)

-- data WMessage = WMessage WHeader [WMessageBlock] BS.ByteString deriving (Eq, Show)

-- data Message = ClientMessage WMessageDescription WMessage | ServerMessage WMessageDescription WMessage deriving (Eq, Show)

data Event = ServerClosedSocket | ClientClosedSocket | SigChld | SigInt
    deriving (Show, Eq)

-- mapping of interface names to interfaces
type InterfaceMap = DM.Map String WInterfaceDescription

-- mapping of bound object ids to interfaces
type ObjectMap = IM.IntMap WInterfaceDescription

data ParsedMessage = UnknownMessage
                   | Message {
                         msgType :: MessageType,
                         msgName :: String,
                         msgInterface :: String,
                         msgArguments :: [MArgument]
                     } deriving (Eq, Show)


dumpByteString :: BS.ByteString -> IO ()
dumpByteString bs = do
        M.mapM_ (\n -> putStr $ N.showHex n " ") bytes
        putStrLn ""
    where
        bytes = BS.unpack bs

{-
loggerThread :: CC.MVar ObjectMap -> STM.TChan Message -> IO ()
loggerThread om chan =

    M.forever $ processData chan

    where
        processData input = do
            x <- STM.atomically $ STM.readTChan input
            case x of
                ClientMessage descr msg@(WMessage header blocks bs) -> do
                        -- dumpMessage descr msg
                        -- putStrLn "request:"
                        -- dumpByteString bs
                        return ()
                ServerMessage descr msg@(WMessage header blocks bs) -> do
                        -- dumpMessage descr msg
                        -- putStrLn "event:"
                        -- dumpByteString bs
                        return ()
-}

intParser :: A.Parser MArgumentValue
intParser = do
    v <- AB.anyWord32le
    return $ MInt $ fromIntegral v

uintParser :: A.Parser MArgumentValue
uintParser = do
    v <- AB.anyWord32le
    return $ MUInt $ fromIntegral v

objectParser :: A.Parser MArgumentValue
objectParser = do
    v <- AB.anyWord32le
    return $ MObject $ fromIntegral v

newIdParser :: String -> A.Parser MArgumentValue
newIdParser interface = do
    v <- AB.anyWord32le
    return $ MNewId (fromIntegral v) interface

fixedParser :: A.Parser MArgumentValue
fixedParser = do
    v <- AB.anyWord32le
    return $ MFixed True 0 $ fromIntegral v -- TODO

stringParser :: A.Parser MArgumentValue
stringParser = do
    lenW <- AB.anyWord32le
    let dataLen = fromIntegral lenW
    let paddedLen = if (rem dataLen 4) == 0
        then dataLen
        else dataLen + (4 - (rem dataLen 4))
    str <- A.take (dataLen - 1)
    A.take 1 -- the terminating NUL byte
    A.take $ paddedLen - dataLen
    return $ MString $ U.toString str

arrayParser :: A.Parser MArgumentValue
arrayParser = do
    lenW <- AB.anyWord32le
    let dataLen = fromIntegral lenW
    let paddedLen = if (rem dataLen 4) == 0
        then dataLen
        else dataLen + (4 - (rem dataLen 4))
    arr <- A.take dataLen
    A.take $ paddedLen - dataLen
    return $ MArray $ arr

fdParser :: A.Parser MArgumentValue
fdParser = return MFd

messageDataParser :: WMessageDescription -> A.Parser [MArgument]
messageDataParser (WMessageDescription _ msgArgs) = do
    let ps = reverse $ messageBlockParser msgArgs []
    values <- M.sequence ps
    let combined = zipWith (\a v -> MArgument (argDescrName a) v) msgArgs values
    return combined
    where
        messageBlockParser :: [WArgumentDescription] -> [A.Parser MArgumentValue] -> [A.Parser MArgumentValue]
        messageBlockParser [] parsers = parsers
        messageBlockParser (arg:args) parsers = messageBlockParser args ((selectParser arg):parsers)
            where
                selectParser :: WArgumentDescription -> A.Parser MArgumentValue
                selectParser a = case argDescrType a of
                    WInt -> intParser
                    WUInt -> uintParser
                    WFd -> fdParser
                    WObject -> objectParser
                    WNewId -> (newIdParser $ argDescrInterface a)
                    WString -> stringParser
                    WArray -> arrayParser
                    WFixed -> fixedParser

messageParser :: ObjectMap -> InterfaceMap -> MessageType -> A.Parser ParsedMessage
messageParser om _ t = do
    senderIdW <- AB.anyWord32le
    opCodeW <- AB.anyWord16le
    msgSizeW <- AB.anyWord16le

    let senderId = fromIntegral senderIdW
    let msgSize = fromIntegral msgSizeW
    let opCode = fromIntegral opCodeW

    case IM.lookup senderId om of
        Just interfaceDescription -> case IM.lookup opCode (getMap t interfaceDescription) of
            Just messageDescription -> do
                let messageName = msgDescrName messageDescription
                let interfaceName = interfaceDescrName interfaceDescription
                args <- messageDataParser messageDescription
                return $ Message t messageName interfaceName args
            Nothing -> do
                A.take (msgSize - 8)
                return UnknownMessage
        Nothing -> do
            A.take (msgSize - 8)
            return UnknownMessage

    where
        getMap :: MessageType -> WInterfaceDescription -> WMessageMap
        getMap messageType interface = case messageType of
            Request -> interfaceRequests interface
            Event -> interfaceEvents interface


isNewId :: MArgument -> Bool
isNewId (MArgument _ (MNewId _ _)) = True
isNewId _ = False

updateMap :: InterfaceMap -> ParsedMessage -> ObjectMap -> ObjectMap
updateMap im msg om =
    case msg of
        Message _ name _ _ ->
            case name of
                "bind" -> case processBind om msg of
                    Just newOm -> newOm
                    Nothing -> om
                "delöete_id" -> case processDeleteId om msg of
                    Just newOm -> newOm
                    Nothing -> om
                _ -> case processCreateObject om msg of
                    Just newOm -> newOm
                    Nothing -> om
        UnknownMessage -> om
    where
        processBind oldOm (Message _ _ _ args) = do
            iface <- List.find (\a -> argName a == "interface") args
            newId <- List.find (\a -> argName a == "id") args
            case argValue iface of
                MString sv -> do
                    interfaceDescr <- DM.lookup sv im
                    case argValue newId of
                        MNewId niv _ -> Just $ IM.insert niv interfaceDescr oldOm
                        _ -> Nothing
                _ -> Nothing

        processCreateObject oldOm (Message _ _ _ args) = do
            newId <- List.find isNewId args
            case argValue newId of
                MNewId niv interface -> do
                    interfaceDescr <- DM.lookup interface im
                    Just $ IM.insert niv interfaceDescr oldOm
                _ -> Nothing

        processDeleteId oldOm (Message _ _ _ args) = do
            deletedId <- List.find (\a -> argName a == "id") args
            case argValue deletedId of
                MUInt v -> do
                    Just $ IM.delete v oldOm
                _ -> Nothing


parseData :: MessageType -> ObjectMap -> InterfaceMap -> BS.ByteString ->
             [ParsedMessage] -> IO (Maybe ([ParsedMessage], ObjectMap))
parseData t om im bs msgs = do
    let res = A.parse (messageParser om im t) bs
    case res of
        A.Fail rest ctx err -> do
            putStrLn $ "Failed to parse: " ++ err
            print ctx
            print rest
            return Nothing
        A.Partial _ -> do
            putStrLn $ "Parsing was a partial match"
            return Nothing
        A.Done i msg -> do
            print msg
            -- update object map
            let newOm = updateMap im msg om
            if BS.null i
                then return $ Just (msgs, newOm)
                else parseData t newOm im i (msg:msgs)


writeToLog msgs bs = undefined


{-}
processBind im msg@(WMessageDescription name args) blocks bs =
    let mIface = List.find (\a -> argName a == "interface") args
        mNewId = List.find (\a -> argName a == "id") args
    in
        do
            case getData mIfaceIndex mNewIdIndex of
                Nothing -> do
                    putStrLn "Error processing bind"
                Just (newId, interface) ->
                    case DM.lookup interface im of
                        Just i -> do
                            insertMapping om (fromIntegral newId) i
                            putStrLn $ "new bind mapping: " ++ show (fromIntegral newId) ++ " -> " ++ interface
                        Nothing -> do
                            putStrLn $ "Interface not found in the map: " ++ interface
    where
        getData :: Maybe Int -> Maybe Int -> Maybe (W.Word32, String)
        getData mInterfaceIndex mNewIdIndex = do
            interfaceIndex <- mInterfaceIndex
            newIdIndex <- mNewIdIndex
            let interfaceBlock = blocks !! interfaceIndex
            let newIdBlock = blocks !! newIdIndex
            interface <- parseMessageString bs $ start interfaceBlock
            newId <- parseMessageWord bs $ start newIdBlock
            return (newId, interface)
-}

loggerThread :: ObjectMap -> InterfaceMap -> STM.TChan (MessageType, BS.ByteString, [Int]) -> IO ()
loggerThread om im chan =

    -- TODO: create mapping (interface, opcode) -> parser

    processData chan om

    where
        processData input objectMap = do
            (t, bs, fds) <- STM.atomically $ STM.readTChan input

            -- Logging file descriptors doesn't make much sense, because the
            -- fd numbers will anyway change when they are passed over the
            -- socket.

            r <- parseData t objectMap im bs []

            case r of
                Just (msgs, newObjectMap) -> do
                    -- writeToLog msgs bs
                    processData chan newObjectMap
                Nothing -> do
                    processData chan objectMap


{-
parseHeader :: BS.ByteString -> (Either String (W.Word32, W.Word16, W.Word16), BS.ByteString)
parseHeader = BG.runGet process
    where
        process :: BG.Get (W.Word32, W.Word16, W.Word16)
        process = do
            senderId <- BG.getWord32host
            msgSize <- BG.getWord16host
            msgOpcode <- BG.getWord16host
            return (senderId, msgSize, msgOpcode)


parseWord :: BS.ByteString -> (Either String W.Word32, BS.ByteString)
parseWord = BG.runGet process
    where
        process :: BG.Get W.Word32
        process = do
            word <- BG.getWord32host
            return word

dumpMessage :: WMessageDescription -> WMessage -> IO ()
dumpMessage (WMessageDescription name args) (WMessage header blocks bs) = do
    putStrLn $ "Message name: " ++ name ++ ", object: " ++ (show $ object header) ++ ", opcode: " ++ (show $ opcode header)
    putStrLn $ "Message size: " ++ show (size header) ++ "/" ++ show (BS.length bs)
    printBlocks args blocks
    putStrLn "Message data:"
    dumpByteString bs

    where
        printBlocks [] [] = return ()
        printBlocks ((WArgumentDescription argName argType argInterface):as) ((WMessageBlock start len _ fd):blocks) = do
            putStrLn $ "  Argument name: " ++ argName
            putStrLn $ "           data length: " ++ show len
            putStrLn $ "           start position: " ++ show start
            case argType of
                WInt -> do
                    putStrLn "           type: Int"
                    let value = show $ fromIntegral $ Maybe.fromJust $ parseMessageWord bs start
                    putStrLn $ "           value: " ++ value
                WUInt -> do
                    putStrLn "           type: UInt"
                    let value = show $ fromIntegral $ Maybe.fromJust $ parseMessageWord bs start
                    putStrLn $ "           value: " ++ value
                WString -> do
                    putStrLn "           type: String"
                    let value = Maybe.fromJust $ parseMessageString bs start
                    putStrLn $ "           value: " ++ value
                WArray -> do
                    putStrLn "           type: Array"
                WNewId -> do
                    putStrLn "           type: NewId"
                    let value = show $ fromIntegral $ Maybe.fromJust $ parseMessageWord bs start
                    putStrLn $ "           value: " ++ value
                WObject -> do
                    putStrLn "           type: Object"
                    let value = show $ fromIntegral $ Maybe.fromJust $ parseMessageWord bs start
                    putStrLn $ "           value: " ++ value
                WFd -> do
                    putStrLn "           type: Object"
                    putStrLn $ "           value: " ++ show fd
                WFixed -> do
                    putStrLn "           type: Fixed"

            printBlocks as blocks


readFd :: Socket.Socket -> IO (Int)
readFd s = do
    putStrLn "readFD"
    fd <- Socket.recvFd s
    putStrLn $ "read FD " ++ show fd
    return $ fromIntegral fd

readPrimitive :: Socket.Socket -> Int -> IO (BS.ByteString)
readPrimitive s len = do
    input <- BSocket.recv s len
    ET.when (BS.null input) $ putStrLn "Socket was closed (TODO: handle this)"
    return input

readArray :: Socket.Socket -> IO (BS.ByteString, Int, Int)
readArray s = do
    firstPart <- BSocket.recv s 4
    ET.when (BS.null firstPart) $ putStrLn "Socket was closed (TODO: handle this)"
    let mSize = parseWord firstPart

    case mSize of
        (Right wsize, _) -> do
            let dataSize = fromIntegral wsize
            let paddedSize = if (rem dataSize 4) == 0
                then dataSize
                else dataSize + (4 - (rem dataSize 4))
            putStrLn $ "reading array of length " ++ show dataSize ++ ", with padding " ++ show paddedSize
            secondPart <- BSocket.recv s paddedSize
            ET.when (BS.null secondPart) $ putStrLn "Socket was closed (TODO: handle this)"
            let fullData = BS.append firstPart secondPart
            return (fullData, dataSize, paddedSize)

        (Left _, _) -> do
            putStrLn "Failed to parse size word (TODO: handle this)"
            return (BS.empty, 0, 0)


readBlocks :: Socket.Socket -> [WArgumentDescription] -> [WMessageBlock] -> BS.ByteString -> Int -> Int -> IO ([WMessageBlock], BS.ByteString)
readBlocks s [] blocks bs start remaining = return (blocks, bs)
readBlocks s (a:as) blocks bs start remaining = do
    case (argDescrType a) of
        WInt -> do
            input <- readPrimitive s 4
            let b = WMessageBlock start 4 WInt 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4) (remaining - 4)
        WUInt -> do
            input <- readPrimitive s 4
            let b = WMessageBlock start 4 WUInt 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4) (remaining - 4)
        WFixed -> do
            input <- readPrimitive s 4
            let b = WMessageBlock start 4 WFixed 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4) (remaining - 4)
        WString -> do
            (input, dataSize, paddedSize) <- readArray s
            let b = WMessageBlock start dataSize WString 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4 + paddedSize) (remaining - paddedSize)
        WObject -> do
            input <- readPrimitive s 4
            let b = WMessageBlock start 4 WObject 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4) (remaining - 4)
        WNewId -> do
            input <- readPrimitive s 4
            let b = WMessageBlock start 4 WNewId 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4) (remaining - 4)
        WArray -> do
            (input, dataSize, paddedSize) <- readArray s
            let b = WMessageBlock start dataSize WString 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4 + paddedSize) (remaining - paddedSize)
        WFd -> do
            putStrLn "Going to read a FD"
            dumpByteString bs
            -- fd <- readFd s
            let b =  WMessageBlock start 0 WFd 0 -- fd
            readBlocks s as (b:blocks) bs start remaining


containsFd :: WMessageDescription -> Bool
containsFd (WMessageDescription _ args) = any (\a -> argDescrType a == WFd) args

readFullMessage :: Socket.Socket -> WHeader -> WMessageDescription -> BS.ByteString -> Int -> IO WMessage
readFullMessage s h d bs remaining = do

    (blocks, bytes) <- readBlocks s (msgDescrArgs d) [] bs 0 remaining

    let dataSize = BS.length bytes - 8

    let missing = remaining - dataSize
    putStrLn $ "read " ++ show (BS.length bytes - 8) ++ " bytes of " ++ show remaining

    if (missing > 0) then do
        extra <- BSocket.recv s missing
        let allBytes = BS.append bytes extra
        putStrLn "ERROR: message size did not match the contents"
        print d
        dumpByteString allBytes
        return $ WMessage h (reverse blocks) allBytes
    else
        return $ WMessage h (reverse blocks) bytes

-}

loop :: MessageType -> Socket.Socket -> Socket.Socket ->
        STM.TChan (MessageType, BS.ByteString, [Int]) -> IO ()
loop t inputSock outputSock logger =  do

    let direction = if t == Request
        then "client"
        else "server"

    (bs, fds) <- recvFromWayland inputSock

    ET.when (BS.null bs) $ ET.throwError $ ET.strMsg $ "input socket for " ++ direction ++ " was closed"

    sent <- sendToWayland outputSock bs fds

    ET.when (sent == 0) $ ET.throwError $ ET.strMsg $ "output socket for " ++ direction ++ " was closed"

    STM.atomically $ STM.writeTChan logger (t, bs, fds)

    loop t inputSock outputSock logger


{-
    -- r is either the resulting msg or an error
    r <- ET.runErrorT $ readData inputSock

    case r of
        (Left err) -> do
            putStrLn $ "Error: " ++ err
        (Right (descr, msg)) -> do
            -- write message to the other socket
            writeData outputSock msg

            -- log the message
            case t of
                Event -> STM.atomically $ T.writeTChan logger (ServerMessage descr msg)
                Request -> STM.atomically $ T.writeTChan logger (ClientMessage descr msg)

            loop im om t inputSock outputSock logger

    return ()
-}

    -- if the socket is no longer connected, end the thread
    -- M.unless (BS.null header) $ processMsg header
{-
    where
        readData :: Socket.Socket -> ET.ErrorT String IO (WMessageDescription, WMessage)
        readData sock = do

            let direction = if t == Request
                then "Request"
                else "Event"

            -- Wayland message header size is 8 bytes.
            header <- ET.liftIO $ BSocket.recv sock 8

            {-
            ET.liftIO $ case t of
                Request -> do
                    putStrLn "read header from client:"
                    dumpByteString header
                Event -> do
                    putStrLn "read header from server:"
                    dumpByteString header
            -}

            ET.when (BS.null header) $ ET.throwError $ ET.strMsg "socket was closed"

            let p = parseHeader header

            let (objectId, opCode, msgSize) = case p of
                   (Left _, _) -> (0, 0, 0)
                   (Right (obj, op, m), _) -> (obj, op, m)
            ET.when (msgSize == 0) $ ET.throwError $ ET.strMsg "parsing error"

            let remaining = fromIntegral $ msgSize - 8


            ET.liftIO $ print (objectId, opCode, msgSize)

            -- get the XML model based on the opcode

            -- TODO: do not make these exceptions fatal

            mInterface <- ET.liftIO $ lookupMapping om (fromIntegral objectId)
            ET.when (Maybe.isNothing mInterface) $ ET.throwError $ ET.strMsg "unknown objectId: " ++ show (fromIntegral objectId)

            let mMessage = IM.lookup (fromIntegral opCode) (getMap t $ Maybe.fromJust mInterface)
            ET.when (Maybe.isNothing mMessage) $ ET.throwError $ ET.strMsg "unknown opcode: " ++ show (fromIntegral opCode)

            let msgDescr = Maybe.fromJust mMessage

            -- read data in blocks so that we don't accidentally go over fd
            let h = WHeader objectId msgSize opCode

            ET.liftIO $ putStrLn $ direction ++ ": " ++ show (interfaceDescrName $ Maybe.fromJust mInterface) ++ " : " ++ show (msgDescrName $ Maybe.fromJust mMessage)

            wmsg@(WMessage header blocks bs) <- ET.liftIO $ readFullMessage inputSock h (Maybe.fromJust mMessage) header remaining

            -- if the message contains new_id blocks, we need to allocate objects for them

            ET.liftIO $ allocateObjects im msgDescr blocks bs

            -- if the server tells that some objects have died, remove them

{-
            input <- ET.liftIO $ BSocket.recv inputSock remaining
            ET.when (BS.null input) $ ET.throwError $ ET.strMsg "socket was closed"

            {-
            ET.liftIO $ putStrLn $ "message header: id=" ++ show objectId ++ ", size=" ++ show msgSize ++ ", opcode=" ++ show opCode
            ET.liftIO $ putStrLn "message content:"
            ET.liftIO $ dumpByteString input
            -}

            let msgData = BS.append header input
            let wmsg = WMessage h [] msgData
-}

            return (msgDescr, wmsg)


        writeData :: Socket.Socket -> WMessage -> IO ()
        writeData sock (WMessage header blocks bs) = do

            -- go through the blocks for FD passing

            CC.threadDelay 1000

            -- dumpByteString bs
            s <- BSocket.send sock bs

            return ()

        getMap :: MessageType -> WInterfaceDescription -> WMessageMap
        getMap messageType interface = case messageType of
            Request -> interfaceRequests interface
            Event -> interfaceEvents interface


        allocateObjects :: InterfaceMap -> WMessageDescription -> [WMessageBlock] -> BS.ByteString -> IO ()
        allocateObjects im msg@(WMessageDescription name args) blocks bs =
            if name == "bind"
                then processBind im msg blocks bs
                else createObjects args blocks bs
            where
                createObjects [] [] bs = return ()
                createObjects (a:as) (b:blocks) bs = do
                    case b of
                        (WMessageBlock start _ WNewId _) -> do
                            let interfaceName = argDescrInterface a
                            let newIdBs = ((BS.take 4) . (BS.drop (8+start))) bs
                            let mNewId = parseWord newIdBs
                            case mNewId of
                                (Right newId, _) -> do
                                    let mInterface = DM.lookup interfaceName im
                                    ET.when (Maybe.isNothing mInterface) $ do
                                        putStrLn $ "failed to find interface from map: " ++ interfaceName
                                        Exit.exitFailure

                                    insertMapping om (fromIntegral newId) (Maybe.fromJust mInterface)
                                    putStrLn $ "new object mapping: " ++ show (fromIntegral newId) ++ " -> " ++ interfaceName
                                    createObjects as blocks bs
                                (Left _, _) -> do
                                    putStrLn "Could not read new id from bytestring"
                            createObjects as blocks bs
                        _ -> createObjects as blocks bs


        processBind im msg@(WMessageDescription name args) blocks bs =
            let mIfaceIndex = List.findIndex (\a -> argDescrName a == "interface") args
                mNewIdIndex = List.findIndex (\a -> argDescrName a == "id") args
            in
                do
                    case getData mIfaceIndex mNewIdIndex of
                        Nothing -> do
                            putStrLn "Error processing bind"
                        Just (newId, interface) ->
                            case DM.lookup interface im of
                                Just i -> do
                                    insertMapping om (fromIntegral newId) i
                                    putStrLn $ "new bind mapping: " ++ show (fromIntegral newId) ++ " -> " ++ interface
                                Nothing -> do
                                    putStrLn $ "Interface not found in the map: " ++ interface
            where
                getData :: Maybe Int -> Maybe Int -> Maybe (W.Word32, String)
                getData mInterfaceIndex mNewIdIndex = do
                    interfaceIndex <- mInterfaceIndex
                    newIdIndex <- mNewIdIndex
                    let interfaceBlock = blocks !! interfaceIndex
                    let newIdBlock = blocks !! newIdIndex
                    interface <- parseMessageString bs $ start interfaceBlock
                    newId <- parseMessageWord bs $ start newIdBlock
                    return (newId, interface)

parseMessageString :: BS.ByteString -> Int -> Maybe String
parseMessageString bs start =
    case mStrLen of
        (Left _, _) -> Nothing
        (Right strLen, _) ->
            -- strLen - 1, since a NUL byte is included in string lenght
            let strBytes = ((BS.take $ (fromIntegral strLen)-1) . (BS.drop (strStart + 4))) bs
            in
                Just $ U.toString strBytes

    where
        strStart = 8 + start :: Int
        strLenBs = ((BS.take 4) . (BS.drop strStart)) bs
        mStrLen = parseWord strLenBs

parseMessageWord :: BS.ByteString -> Int -> Maybe W.Word32
parseMessageWord bs start =
    case mWord of
        (Left _, _) -> Nothing
        (Right word, _) -> Just word

    where
        wordStart = 8 + start
        wordBs = ((BS.take 4) . (BS.drop wordStart)) bs
        mWord = parseWord wordBs
-}

{-
Some objects are so called "typeless objects". The scanner generates extra code
for them, meaning that the message description in the xml protocol files is not
an accurate description of the message content on the wire.
-}

isTypelessObject :: WMessageDescription -> Bool
isTypelessObject (WMessageDescription _ args) = any typeless args
    where typeless (WArgumentDescription _ t i) = if t == WNewId && i == ""
            then True
            else False

fixMessage :: WMessageDescription -> WMessageDescription
fixMessage msg@(WMessageDescription n args) =
    if isTypelessObject msg
        then
            -- insert new fields before the new_id parameter
            let beginning = takeWhile (\a -> argDescrType a /= WNewId) args
                end = dropWhile (\a -> argDescrType a /= WNewId) args
                newArgs = [ WArgumentDescription "interface" WString "",
                            WArgumentDescription "version" WUInt "" ]
            in
                WMessageDescription n (beginning ++ newArgs ++ end)
        else
            msg

fixInterface (WInterfaceDescription n rs es) =
    WInterfaceDescription n (IM.map fixMessage rs) (IM.map fixMessage es)

clientThread :: STM.TMVar Event ->
                Socket.Socket -> Socket.Socket ->
                STM.TChan (MessageType, BS.ByteString, [Int]) -> IO ()
clientThread eventV clientSock serverSock loggerChan = do
    loop Request clientSock serverSock loggerChan
    STM.atomically $ STM.putTMVar eventV ClientClosedSocket


serverThread :: STM.TMVar Event ->
                Socket.Socket -> Socket.Socket ->
                STM.TChan (MessageType, BS.ByteString, [Int]) -> IO ()
serverThread eventV serverSock clientSock loggerChan = do
    loop Event serverSock clientSock loggerChan
    STM.atomically $ STM.putTMVar eventV ServerClosedSocket


execProcess :: FilePath -> [String] -> Socket.Socket -> IO a
execProcess path args sock = do
    let fd = show $ Socket.fdSocket sock

    env <- E.getEnvironment
    let filteredEnv = filter (\x -> fst x /= "WAYLAND_SOCKET") env

    putStrLn $ "Exec " ++ path ++ " with WAYLAND_SOCKET=" ++ fd
    Process.executeFile path True args (Just $ ("WAYLAND_SOCKET", fd):filteredEnv)
    -- Process.executeFile path True args (Just filteredEnv)

createXdgPath :: a -> IO String
createXdgPath _ = do
    userid <- PU.getRealUserID
    return $ "/var/run/" ++ show userid

sigHandler :: Signals.Signal -> STM.TMVar Event -> IO ()
sigHandler sig var = do
    let e = if sig == Signals.sigINT
        then SigInt
        else SigChld
    STM.atomically $ STM.putTMVar var e


readXmlData :: [FilePath] -> InterfaceMap -> IO (Maybe (InterfaceMap))
readXmlData [] mapping = return $ Just mapping
readXmlData (xf:xfs) mapping = do
    d <- readFile xf
    let newMapping = addMapping d mapping
    case newMapping of
        Nothing -> return Nothing
        Just m -> readXmlData xfs m

    where
        addMapping :: String -> InterfaceMap -> Maybe InterfaceMap
        addMapping d imap = do
            is <- parseWaylandXML d
            let fixedIs = map fixInterface is
            let m = foldr (\i -> DM.insert (interfaceDescrName i) i) (DM.empty) fixedIs
            let exists = not $ DM.null $ DM.intersection m imap
            if exists
                then Nothing
                else Just $ DM.union imap m


-- functions for handling shared state in object map

lookupMapping :: CC.MVar ObjectMap -> Int -> IO (Maybe WInterfaceDescription)
lookupMapping m objectId = do
    mapping <- CC.takeMVar m
    CC.putMVar m mapping
    return $ IM.lookup objectId mapping

insertMapping :: CC.MVar ObjectMap -> Int -> WInterfaceDescription -> IO ()
insertMapping m objectId interface = do
    mapping <- CC.takeMVar m
    let newMapping = IM.insert objectId interface mapping
    CC.putMVar m newMapping

removeMapping :: CC.MVar ObjectMap -> Int -> IO ()
removeMapping m objectId = do
    mapping <- CC.takeMVar m
    let newMapping = IM.delete objectId mapping
    CC.putMVar m newMapping


runApplication :: [String] -> String -> Maybe String -> String -> [String] -> IO ()
runApplication xfs lt lf cmd cmdargs = do

    -- read the protocol file(s)

    maybeInterfaceMap <- readXmlData xfs DM.empty

    ET.when (Maybe.isNothing maybeInterfaceMap) $ do
        putStrLn "reading or parsing of XML files failed"
        Exit.exitFailure

    let interfaceMap = Maybe.fromJust maybeInterfaceMap

    -- check that the global object wl_display exists in the map

    let maybeDisplay = DM.lookup "wl_display" interfaceMap

    ET.when (Maybe.isNothing maybeDisplay) $ do
        putStrLn "required global wl_display not found"
        Exit.exitFailure

    let display = Maybe.fromJust maybeDisplay

    -- initialize object map with known global mapping 1 -> "wl_display"

    let objectMap = IM.insert 1 display IM.empty

    -- read the WAYLAND_DISPLAY environment variable

    loggerChan <- STM.newTChanIO
    eventV <- STM.newEmptyTMVarIO

    _ <- Signals.installHandler Signals.sigINT (Signals.Catch $ sigHandler Signals.sigINT eventV) Nothing
    _ <- Signals.installHandler Signals.sigCHLD (Signals.Catch $ sigHandler Signals.sigCHLD eventV) Nothing

    _ <- CC.forkIO $ loggerThread objectMap interfaceMap loggerChan

    xdgDir <- Err.catchIOError (E.getEnv "XDG_RUNTIME_DIR") createXdgPath
    serverName <- Err.catchIOError (E.getEnv "WAYLAND_DISPLAY") (\_ -> return "wayland-0")

    -- open the connection to the server

    serverSock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    PI.setFdOption (PT.Fd $ Socket.fdSocket serverSock) PI.CloseOnExec True

    let serverPath = xdgDir ++ "/" ++ serverName

    putStrLn $ "Connecting to " ++ serverPath

    Socket.connect serverSock (Socket.SockAddrUnix serverPath)

    -- create socket for the child and start a thread for it

    (clientSock, trackerSock) <- Socket.socketPair Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    PI.setFdOption (PT.Fd $ Socket.fdSocket clientSock) PI.CloseOnExec True

    -- start threads for communication

    _ <- CC.forkIO $ serverThread eventV serverSock clientSock loggerChan

    _ <- CC.forkIO $ clientThread eventV clientSock serverSock loggerChan

    -- fork the child

    pid <- Process.forkProcess $ execProcess cmd cmdargs trackerSock

    Socket.close trackerSock

    -- process messages until the child dies (closes the socket), server dies or
    -- there is a SIGINT

    M.forever $ do
        e <- STM.atomically $ STM.takeTMVar eventV

        case e of
            SigInt -> do
                putStrLn "sigINT received"
                Exit.exitSuccess
            SigChld -> do
                putStrLn "sigCHLD received"
                Exit.exitSuccess
            ServerClosedSocket -> do
                putStrLn "server closed socket"
                Signals.signalProcess Signals.sigINT pid
                Exit.exitFailure
            ClientClosedSocket -> do
                putStrLn "client closed socket"
                Exit.exitSuccess

