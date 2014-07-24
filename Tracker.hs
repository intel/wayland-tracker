module Tracker where

-- import qualified Data.Word as W
-- import qualified Foreign.C.Types as C
-- import qualified Data.Binary.Strict.Get as BG
import qualified Control.Monad as M
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BSL
import qualified System.IO as IO
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
-- import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Clock

import Control.Applicative

import Types
import Log
import ParseWaylandXML
import Wayland

-- import Debug.Trace

data Event = ServerClosedSocket | ClientClosedSocket | SigChld | SigInt
    deriving (Show, Eq)

-- mapping of interface names to interfaces
type InterfaceMap = DM.Map String WInterfaceDescription

-- mapping of bound object ids to interfaces
type ObjectMap = IM.IntMap WInterfaceDescription


getLogType :: String -> Maybe LogType
getLogType s = case s of
    "simple" -> Just Simple
    "binary" -> Just Binary
    "json" -> Just Json
    _ -> Nothing


generateTS :: IO String
generateTS = do
    time <- Clock.getPOSIXTime
    return $ "[" ++ (show time) ++ "]"

dumpByteString :: BS.ByteString -> IO ()
dumpByteString bs = do
        M.mapM_ (\n -> putStr $ N.showHex n " ") bytes
        putStrLn ""
    where
        bytes = BS.unpack bs


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


binaryMessageParser:: MessageType -> A.Parser ParsedBinaryMessage
binaryMessageParser t = do
    senderIdW <- AB.anyWord32le
    opCodeW <- AB.anyWord16le
    msgSizeW <- AB.anyWord16le

    let senderId = fromIntegral senderIdW
    let size = fromIntegral msgSizeW
    let opCode = fromIntegral opCodeW


    msgData <- A.take (size - 8)

    return $ ParsedBinaryMessage t senderId size opCode msgData

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
                "delete_id" -> case processDeleteId om msg of
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
                    then Right ((msg:msgs), newOm)
                    else parseData t newOm im i (msg:msgs)


processingThread :: ObjectMap -> InterfaceMap ->
                    STM.TChan (MessageType, BS.ByteString, [Int]) ->
                    IO.Handle -> LogType -> IO ()
processingThread om im chan lh lt = processData chan om
    where
        logger = Logger lh lt
        processData input objectMap = do
            (t, bs, _) <- STM.atomically $ STM.readTChan input

            -- Everything read in one go will have the same timestamp

            ts <- generateTS

            -- Logging file descriptors doesn't make much sense, because the
            -- fd numbers will anyway change when they are passed over the
            -- socket.

            -- TODO: just do binary parsing if lf = Binary

            case lt of
                Binary -> do
                    let r = parseBinaryData t bs []
                    case r of
                        Right msgs -> do
                            mapM_ (writeBinaryLog logger ts) msgs
                            processData chan om
                        Left str -> do
                            putStrLn str
                            processData chan om
                _ ->
                    let r = parseData t objectMap im bs []
                    in
                        case r of
                            Right (msgs, newObjectMap) -> do
                                -- writeToLog msgs bs
                                putStrLn $ "parsed " ++ (show $ length msgs) ++ " messages"
                                mapM_ (writeLog logger ts) msgs
                                processData chan newObjectMap
                            Left str -> do
                                putStrLn str
                                processData chan objectMap


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


fixInterface :: WInterfaceDescription -> WInterfaceDescription
fixInterface (WInterfaceDescription n rs es) =
    WInterfaceDescription n (IM.map fixMessage rs) (IM.map fixMessage es)


clientThread :: STM.TMVar Event -> Socket.Socket -> Socket.Socket ->
                STM.TChan (MessageType, BS.ByteString, [Int]) -> IO ()
clientThread eventV clientSock serverSock loggerChan = do
    loop Request clientSock serverSock loggerChan
    STM.atomically $ STM.putTMVar eventV ClientClosedSocket


serverThread :: STM.TMVar Event -> Socket.Socket -> Socket.Socket ->
                STM.TChan (MessageType, BS.ByteString, [Int]) -> IO ()
serverThread eventV serverSock clientSock loggerChan = do
    loop Event serverSock clientSock loggerChan
    STM.atomically $ STM.putTMVar eventV ServerClosedSocket


execProcess :: FilePath -> [String] -> Socket.Socket -> IO a
execProcess path args sock = do
    let fd = show $ Socket.fdSocket sock

    env <- E.getEnvironment
    let filteredEnv = filter (\x -> fst x /= "WAYLAND_SOCKET") env

    -- TODO: channel client stdout, stderr to this process' stderr

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


runApplication :: [String] -> String -> Maybe String -> String -> [String] -> IO ()
runApplication xfs lt lf cmd cmdargs = do

    let logFormat = getLogType lt

    ET.when (Maybe.isNothing logFormat) $ do
        putStrLn $ "unknown log format type " ++ lt ++ "; known types are simple, binary and json"
        Exit.exitFailure

    logHandle <- if Maybe.isNothing lf
        then return IO.stdout
        else IO.openFile (Maybe.fromJust lf) IO.ReadMode

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

    _ <- CC.forkIO $ processingThread objectMap interfaceMap loggerChan logHandle (Maybe.fromJust logFormat)

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

