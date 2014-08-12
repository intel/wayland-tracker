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

module Tracker where

import qualified Control.Concurrent         as CC
import qualified Control.Concurrent.STM     as STM
import qualified Control.Monad              as M
import qualified Data.Attoparsec.Binary     as AB
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Binary                as B
import qualified Data.Binary.Bits.Get       as BBG
import qualified Data.Binary.Get            as BG
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.UTF8       as U
import qualified Data.IntMap                as IM
import qualified Data.List                  as List
import qualified Data.Map.Strict            as DM
import qualified Data.Maybe                 as Maybe
import qualified Data.Time.Clock            as Clock
import qualified Data.Word                  as W
import qualified Network.Socket             as Socket
import qualified System.Endian              as Endian
import qualified System.Environment         as E
import qualified System.Exit                as Exit
import qualified System.IO                  as IO
import qualified System.IO.Error            as Err
import qualified System.Posix.IO            as PI
import qualified System.Posix.Process       as Process
import qualified System.Posix.Signals       as Signals
import qualified System.Posix.Types         as PT
import qualified System.Posix.User          as PU

import           Log
import           ParseWaylandXML
import           Types
import           Wayland

data Event = ServerClosedSocket
           | ClientClosedSocket
           | ProcessingEnded
           | SigChld
           | SigInt
    deriving (Show, Eq)

-- mapping of interface names to interfaces
type InterfaceMap = DM.Map String WInterfaceDescription

-- mapping of bound object ids to interfaces
type ObjectMap = IM.IntMap WInterfaceDescription


-- read values in correct endianness

anyWord32he :: A.Parser W.Word32
anyWord32he =
    case Endian.getSystemEndianness of
        Endian.LittleEndian -> AB.anyWord32le
        Endian.BigEndian -> AB.anyWord32be


anyWord16he :: A.Parser W.Word16
anyWord16he =
    case Endian.getSystemEndianness of
        Endian.LittleEndian -> AB.anyWord16le
        Endian.BigEndian -> AB.anyWord16be


putStrLnErr :: String -> IO ()
putStrLnErr s = do
    IO.hPutStr IO.stderr ("wayland-tracker: " ++ s)
    IO.hPutStr IO.stderr "\n"


{-
-- debug: dump a bytestring in hex format to stdout
dumpByteString :: BS.ByteString -> IO ()
dumpByteString bs = do
    M.mapM_ (\n -> putStr $ N.showHex n " ") bytes
    putStrLn ""
    where
        bytes = BS.unpack bs
-}


intParser :: A.Parser MArgumentValue
intParser = do
    v <- anyWord32he
    return $ MInt $ fromIntegral v


uintParser :: A.Parser MArgumentValue
uintParser = do
    v <- anyWord32he
    return $ MUInt $ fromIntegral v


objectParser :: A.Parser MArgumentValue
objectParser = do
    v <- anyWord32he
    return $ MObject $ fromIntegral v


newIdParser :: String -> A.Parser MArgumentValue
newIdParser interface = do
    v <- anyWord32he
    return $ MNewId (fromIntegral v) interface


fixedParser :: A.Parser MArgumentValue
fixedParser = do
    v <- anyWord32he
    let bs = B.encode v
    let (sign, f, s) = getFixedValues bs
    return $ MFixed sign f s
    where
        getFixedValues :: BSL.ByteString -> (Bool, Int, Int)
        getFixedValues = BG.runGet $ do
            (a, b, c) <- BBG.runBitGet parseFixed
            return (a,b,c)

        parseFixed :: BBG.BitGet (Bool, Int, Int)
        parseFixed = do
            sign <- BBG.getBool
            f <- BBG.getWord32be 23
            s <- BBG.getWord8 8
            return (sign, fromIntegral f, fromIntegral s)


paddedLength :: Int -> Int
paddedLength len = if rem len 4 == 0
    then len
    else len + 4 - rem len 4


stringParser :: A.Parser MArgumentValue
stringParser = do
    lenW <- anyWord32he
    let dataLen = fromIntegral lenW
    if dataLen == 0 -- this is a null string, it's not even an empty string
        then return $ MString "(null)" -- TODO: better representation?
        else do
            str <- A.take (dataLen - 1)
            A.take 1 -- the terminating NUL byte
            A.take $ (paddedLength dataLen) - dataLen -- read padded bytes
            return $ MString $ U.toString str


arrayParser :: A.Parser MArgumentValue
arrayParser = do
    lenW <- anyWord32he
    let dataLen = fromIntegral lenW
    arr <- A.take dataLen
    A.take $ (paddedLength dataLen) - dataLen
    return $ MArray arr


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
        messageBlockParser (arg:args) parsers = messageBlockParser args (selectParser arg:parsers)
            where
                selectParser :: WArgumentDescription -> A.Parser MArgumentValue
                selectParser a = case argDescrType a of
                    WInt -> intParser
                    WUInt -> uintParser
                    WFd -> fdParser
                    WObject -> objectParser
                    WNewId -> newIdParser $ argDescrInterface a
                    WString -> stringParser
                    WArray -> arrayParser
                    WFixed -> fixedParser


messageParser :: ObjectMap -> InterfaceMap -> MessageType -> A.Parser ParsedMessage
messageParser om _ t = do
    senderIdW <- anyWord32he
    opCodeW <- anyWord16he
    msgSizeW <- anyWord16he

    let sId = fromIntegral senderIdW
    let size = fromIntegral msgSizeW
    let op = fromIntegral opCodeW

    case IM.lookup sId om of
        Just interfaceDescription -> case IM.lookup op (getMap t interfaceDescription) of
            Just messageDescription -> do
                let messageName = msgDescrName messageDescription
                let interfaceName = interfaceDescrName interfaceDescription
                args <- messageDataParser messageDescription
                return $ Message t messageName interfaceName sId args
            Nothing -> do
                A.take (size - 8)
                return $ UnknownMessage t
        Nothing -> do
            A.take (size - 8)
            return $ UnknownMessage t

    where
        getMap :: MessageType -> WInterfaceDescription -> WMessageMap
        getMap messageType interface = case messageType of
            Request -> interfaceRequests interface
            Event -> interfaceEvents interface


binaryMessageParser:: MessageType -> A.Parser ParsedBinaryMessage
binaryMessageParser t = do
    senderIdW <- anyWord32he
    opCodeW <- anyWord16he
    msgSizeW <- anyWord16he

    let sId = fromIntegral senderIdW
    let size = fromIntegral msgSizeW
    let op = fromIntegral opCodeW

    msgBs <- A.take (size - 8)

    return $ ParsedBinaryMessage t sId op size msgBs


isNewId :: MArgument -> Bool
isNewId (MArgument _ (MNewId _ _)) = True
isNewId _ = False


updateMap :: InterfaceMap -> ParsedMessage -> ObjectMap -> ObjectMap
updateMap im msg om =
    case msg of
        Message _ name _ _ _ ->
            case name of
                "bind" -> Maybe.fromMaybe om (processBind om msg)
                "delete_id" -> Maybe.fromMaybe om (processDeleteId om msg)
                _ -> Maybe.fromMaybe om (processCreateObject om msg)
        UnknownMessage _ -> om
    where
        processBind oldOm (Message _ _ _ _ args) = do
            iface <- List.find (\a -> argName a == "interface") args
            newId <- List.find (\a -> argName a == "id") args
            case argValue iface of
                MString sv -> do
                    interfaceDescr <- DM.lookup sv im
                    case argValue newId of
                        MNewId niv _ -> Just $ IM.insert niv interfaceDescr oldOm
                        _ -> Nothing
                _ -> Nothing

        processCreateObject oldOm (Message _ _ _ _ args) = do
            newId <- List.find isNewId args
            case argValue newId of
                MNewId niv interface -> do
                    interfaceDescr <- DM.lookup interface im
                    Just $ IM.insert niv interfaceDescr oldOm
                _ -> Nothing

        processDeleteId oldOm (Message _ _ _ _ args) = do
            deletedId <- List.find (\a -> argName a == "id") args
            case argValue deletedId of
                MUInt v -> Just $ IM.delete v oldOm
                _ -> Nothing


parseBinaryData :: MessageType -> BS.ByteString -> [ParsedBinaryMessage] ->
                   Either String [ParsedBinaryMessage]
parseBinaryData t bs msgs =
    case A.parse (binaryMessageParser t) bs of
        A.Fail _ _ err -> Left ("Parsing failure: " ++ err)
        A.Partial _ -> Left "Confused with protocol files and actual data"
        A.Done i msg ->
            if BS.null i
                then Right (msg:msgs)
                else parseBinaryData t i (msg:msgs)


parseData :: MessageType -> ObjectMap -> InterfaceMap -> BS.ByteString ->
             [ParsedMessage] -> Either String ([ParsedMessage], ObjectMap)
parseData t om im bs msgs =
    case A.parse (messageParser om im t) bs of
        A.Fail _ _ err -> Left ("Parsing failure: " ++ err)
        A.Partial _ -> Left "Confused with protocol files and actual data"
        A.Done i msg ->
            -- update object map
            let newOm = updateMap im msg om
            in
                if BS.null i
                    then Right (msg:msgs, newOm)
                    else parseData t newOm im i (msg:msgs)


processingThread :: STM.TMVar Event ->
                    (Clock.UTCTime -> Clock.NominalDiffTime) -> [String] ->
                    STM.TChan (Maybe (MessageType, BS.ByteString, [Int])) ->
                    IO.Handle -> LogType -> IO ()
processingThread eventV ts xfs chan lh lt = do

    -- read the protocol file(s)

    case lt of
        Binary -> processBinaryData chan
        _ -> do
            xmlData <- readXmlData xfs DM.empty
            case getXmlData xmlData of
                Nothing -> putStrLnErr "reading or parsing of XML files failed"
                Just (im, displayDescr) -> do
                    -- initialize object map with known global mapping 1 -> "wl_display"
                    let objectMap = IM.insert 1 displayDescr IM.empty
                    processData chan objectMap im

    -- send an error message to the channel if we end here: it means
    -- that something has gone wrong with the XML files or that the main
    -- program has asked us to quit.

    STM.atomically $ STM.putTMVar eventV ProcessingEnded

    where
        getXmlData xmlData = do
            interfaceMap <- xmlData
            displayDescr <- DM.lookup "wl_display" interfaceMap
            return (interfaceMap, displayDescr)

        logger = Logger lh lt

        processBinaryData input = do
            v <- STM.atomically $ STM.readTChan input

            case v of
                Nothing -> return () -- time to end this thread
                Just (t, bs, _) -> do

                    -- Everything read in one go will have the same timestamp
                    currentTime <- Clock.getCurrentTime
                    let timeStamp = ts currentTime

                    let r = parseBinaryData t bs []
                    case r of
                        Right msgs -> do
                            mapM_ (writeBinaryLog logger timeStamp) msgs
                            processBinaryData chan
                        Left str -> do
                            putStrLnErr str
                            processBinaryData chan

        processData input objectMap im = do
            v <- STM.atomically $ STM.readTChan input

            case v of
                Nothing -> return () -- time to end this thread
                Just (t, bs, _) -> do

                    currentTime <- Clock.getCurrentTime
                    let timeStamp = ts currentTime

                    -- Logging file descriptors doesn't make much sense, because
                    -- the fd numbers will anyway change when they are passed
                    -- over the socket.

                    let r = parseData t objectMap im bs []
                    case r of
                        Right (msgs, newObjectMap) -> do
                            mapM_ (writeLog logger timeStamp) msgs
                            processData chan newObjectMap im
                        Left str -> do
                            putStrLnErr str
                            processData chan objectMap im


rwloop :: MessageType -> Socket.Socket -> Socket.Socket ->
        STM.TChan (Maybe (MessageType, BS.ByteString, [Int])) -> IO Event
rwloop t inputSock outputSock logger =  do

    (bs, fds) <- Err.catchIOError (recvFromWayland inputSock) (\_ -> return (BS.empty, []))

    if BS.null bs
        then if t == Request then return ClientClosedSocket else return ServerClosedSocket
        else do
            sent <- Err.catchIOError (sendToWayland outputSock bs fds) (\_ -> return 0)
            if sent == 0
                then if t == Event then return ClientClosedSocket else return ServerClosedSocket
                else do
                    STM.atomically $ STM.writeTChan logger $ Just (t, bs, fds)
                    rwloop t inputSock outputSock logger


{-
Some objects are so called "typeless objects". The scanner generates extra code
for them, meaning that the message description in the xml protocol files is not
an accurate description of the message content on the wire.
-}

isTypelessObject :: WMessageDescription -> Bool
isTypelessObject (WMessageDescription _ args) = any typeless args
    where typeless (WArgumentDescription _ t i) = t == WNewId && i == ""


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


fixInterface :: WInterfaceDescription -> WInterfaceDescription
fixInterface (WInterfaceDescription n rs es) =
    WInterfaceDescription n (IM.map fixMessage rs) (IM.map fixMessage es)


clientThread :: STM.TMVar Event -> Socket.Socket -> Socket.Socket ->
                STM.TChan (Maybe (MessageType, BS.ByteString, [Int])) -> IO ()
clientThread eventV clientSock serverSock loggerChan = do
    event <- rwloop Request clientSock serverSock loggerChan
    STM.atomically $ STM.putTMVar eventV event


serverThread :: STM.TMVar Event -> Socket.Socket -> Socket.Socket ->
                STM.TChan (Maybe (MessageType, BS.ByteString, [Int])) -> IO ()
serverThread eventV serverSock clientSock loggerChan = do
    event <- rwloop Event serverSock clientSock loggerChan
    STM.atomically $ STM.putTMVar eventV event


-- ioThread reads input from child and outputs it to stderr
ioThread :: PT.Fd -> IO ()
ioThread fd = M.forever loop
    where
        loop = do
            CC.threadWaitRead $ fromIntegral fd
            (str, _) <- PI.fdRead fd 4096
            IO.hPutStr IO.stderr str


execProcess :: FilePath -> [String] -> Socket.Socket -> PT.Fd -> IO a
execProcess path args sock fd = do
    let wFd = show $ Socket.fdSocket sock

    env <- E.getEnvironment
    let filteredEnv = filter (\x -> fst x /= "WAYLAND_SOCKET") env

    -- channel client stdout, stderr to this process' stderr

    -- TODO: we need to handle stdin also, but in a separate thread

    -- IO.hClose IO.stdin
    IO.hClose IO.stdout
    IO.hClose IO.stderr

    -- PI.dupTo fd PI.stdInput
    PI.dupTo fd PI.stdOutput
    PI.dupTo fd PI.stdError

    PI.closeFd fd

    -- putStrLnErr $ "Exec " ++ path ++ " with WAYLAND_SOCKET=" ++ fd
    Process.executeFile path True args (Just $ ("WAYLAND_SOCKET", wFd):filteredEnv)


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


readXmlData :: [FilePath] -> InterfaceMap -> IO (Maybe InterfaceMap)
readXmlData [] mapping = return $ Just mapping
readXmlData (xf:xfs) mapping = do
    h <- IO.openFile xf IO.ReadMode
    -- set encoding, because Wayland XML files are UTF8
    IO.hSetEncoding h IO.utf8
    d <- IO.hGetContents h
    case addMapping d mapping of
        Nothing -> do
            IO.hClose h
            return Nothing
        Just m -> do
            IO.hClose h
            readXmlData xfs m

    where
        addMapping :: String -> InterfaceMap -> Maybe InterfaceMap
        addMapping d imap = do
            is <- parseWaylandXML d
            let fixedIs = map fixInterface is
            let m = foldr (\i -> DM.insert (interfaceDescrName i) i) DM.empty fixedIs
            let exists = not $ DM.null $ DM.intersection m imap
            if exists
                then Nothing
                else Just $ DM.union imap m


timeSinceStart :: Clock.UTCTime -> Clock.UTCTime -> Clock.NominalDiffTime
timeSinceStart beginning current = Clock.diffUTCTime current beginning


runApplication :: [String] -> LogType -> Maybe String -> String -> [String] -> IO ()
runApplication xfs lt lf cmd cmdargs = do

    logHandle <- if Maybe.isNothing lf
        then return IO.stdout
        else IO.openFile (Maybe.fromJust lf) IO.WriteMode

    beginning <- Clock.getCurrentTime
    let ts = timeSinceStart beginning

    -- read the WAYLAND_DISPLAY environment variable

    loggerChan <- STM.newTChanIO
    eventV <- STM.newEmptyTMVarIO

    _ <- Signals.installHandler Signals.sigINT (Signals.Catch $ sigHandler Signals.sigINT eventV) Nothing
    _ <- Signals.installHandler Signals.sigCHLD (Signals.Catch $ sigHandler Signals.sigCHLD eventV) Nothing

    xdgDir <- Err.catchIOError (E.getEnv "XDG_RUNTIME_DIR") createXdgPath
    serverName <- Err.catchIOError (E.getEnv "WAYLAND_DISPLAY") (\_ -> return "wayland-0")

    -- open the connection to the server

    serverSock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    PI.setFdOption (PT.Fd $ Socket.fdSocket serverSock) PI.CloseOnExec True

    let serverPath = xdgDir ++ "/" ++ serverName

    putStrLnErr $ "connecting to " ++ serverPath

    Socket.connect serverSock (Socket.SockAddrUnix serverPath)

    -- create stdin/stdout/stderr socket pair for the child
    (ourReadFd, childWriteFd) <- PI.createPipe
    PI.setFdOption ourReadFd PI.CloseOnExec True

    -- create wayland socket for the child and start a thread for it

    (clientSock, trackerSock) <- Socket.socketPair Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    PI.setFdOption (PT.Fd $ Socket.fdSocket clientSock) PI.CloseOnExec True

    -- start threads for communication and processing

    pt <- CC.forkIO $ processingThread eventV ts xfs loggerChan logHandle lt
    st <- CC.forkIO $ serverThread eventV serverSock clientSock loggerChan
    ct <- CC.forkIO $ clientThread eventV clientSock serverSock loggerChan
    rt <- CC.forkIO $ ioThread ourReadFd

    -- fork the child

    pid <- Process.forkProcess $ execProcess cmd cmdargs trackerSock childWriteFd

    Socket.close trackerSock

    -- process messages until the child dies (closes the socket), server dies or
    -- there is a SIGINT

    M.forever $ do
        e <- STM.atomically $ STM.takeTMVar eventV

        case e of
            SigInt -> do
                putStrLnErr "sigINT received"
                -- wait until the logger thread finishes
                STM.atomically $ STM.writeTChan loggerChan Nothing
            SigChld -> do
                putStrLnErr "sigCHLD received"
                STM.atomically $ STM.writeTChan loggerChan Nothing
            ServerClosedSocket -> do
                putStrLnErr "server closed socket"
                Signals.signalProcess Signals.sigINT pid
                STM.atomically $ STM.writeTChan loggerChan Nothing
            ClientClosedSocket -> do
                putStrLnErr "client closed socket"
                STM.atomically $ STM.writeTChan loggerChan Nothing
            ProcessingEnded -> do
                putStrLnErr "exiting"

                IO.hClose logHandle
                CC.killThread pt
                CC.killThread st
                CC.killThread ct
                CC.killThread rt

                Socket.close clientSock
                Socket.close serverSock

                -- finally exit when the logger thread is done
                Exit.exitSuccess
