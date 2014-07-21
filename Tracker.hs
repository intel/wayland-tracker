module Tracker where

import qualified Data.Word as W
-- import qualified Foreign.C.Types as C
import qualified Data.Binary.Strict.Get as BG
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
import qualified Network.Socket.ByteString as BSocket
-- import qualified System.IO as SIO
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as T
import qualified Control.Monad.Error as ET
import qualified Numeric as N
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as DM
import qualified Data.IntMap as IM

import ParseWaylandXML

-- messages contain header and a list of data / fd blocks

data WHeader = WHeader { object :: W.Word32, size :: W.Word16, opcode :: W.Word16 }

data WMessageBlock = WMessageBlock {
    start :: Int,
    dataLength :: Int,
    blockType :: WArgumentType,
    dataFd :: Int
}

data MessageType = Request | Event

type WXmlModel = [WArgumentType]

type WMessageModel = [WMessageBlock]

data WMessage = WMessage WHeader [WMessageBlock] BS.ByteString

data Message = ClientMessage WMessage | ServerMessage WMessage

data Event = ServerClosedSocket | ClientClosedSocket | SigChld | SigInt
    deriving (Show, Eq)

-- mapping of interface names to interfaces
type InterfaceMap = DM.Map String WInterfaceDescription

-- mapping of bound object ids to interfaces
type ObjectMap = IM.IntMap WInterfaceDescription

dumpByteString :: BS.ByteString -> IO ()
dumpByteString bs = do
        M.mapM_ (\n -> putStr $ N.showHex n " ") bytes
        putStrLn ""
    where
        bytes = BS.unpack bs

loggerThread :: CC.MVar ObjectMap -> STM.TChan Message -> IO ()
loggerThread om chan =

    M.forever $ processData chan

    where
        processData input = do
            x <- STM.atomically $ STM.readTChan input
            case x of
                ClientMessage (WMessage header blocks bs) -> do
                        putStrLn "request:"
                        dumpByteString bs
                ServerMessage (WMessage header blocks bs) -> do
                        putStrLn "event:"
                        dumpByteString bs

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


readFd :: Socket.Socket -> IO (Int)
readFd s = do
    fd <- Socket.recvFd s
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
            let paddedSize = dataSize + (4 - (rem dataSize 4))
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
        WUint -> do
            input <- readPrimitive s 4
            let b = WMessageBlock start 4 WUint 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4) (remaining - 4)
        WFixed -> do
            input <- readPrimitive s 4
            let b = WMessageBlock start 4 WFixed 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + 4) (remaining - 4)
        WString -> do
            (input, dataSize, paddedSize) <- readArray s
            let b = WMessageBlock start dataSize WString 0
            readBlocks s as (b:blocks) (BS.append bs input) (start + paddedSize) (remaining - paddedSize)
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
            readBlocks s as (b:blocks) (BS.append bs input) (start + paddedSize) (remaining - paddedSize)
        WFd -> do
            fd <- readFd s
            let b =  WMessageBlock start 0 WFd fd
            readBlocks s as (b:blocks) bs start remaining


readFullMessage :: Socket.Socket -> WHeader -> WMessageDescription -> BS.ByteString -> Int -> IO WMessage
readFullMessage s h d bs remaining = do
    (blocks, bytes) <- readBlocks s (msgDescrArgs d) [] bs 0 remaining
    return $ WMessage h (reverse blocks) bytes

loop :: CC.MVar ObjectMap -> MessageType -> Socket.Socket -> Socket.Socket -> T.TChan Message -> IO ()
loop om t inputSock outputSock logger =  do
    -- read from client socket
{-
    putStrLn "recvfd from socket"
    fd <- Err.catchIOError (Socket.recvFd inputSock) (\_ -> return (-1))
    putStrLn $ "result: " ++ show fd
-}

    -- Some messages contain file descriptors. We cannot just read
    -- all there is because the file descriptors get closed by kernel.
    -- Instead, we need to read until we know the message format and then
    -- read until the fd. The fd is then received via recvFd.

    -- Wayland message header size is 8 bytes.

    -- r is either the resulting msg or an error
    r <- ET.runErrorT $ readData inputSock

    case r of
        (Left _) -> return ()
        (Right msg) -> do
            -- write message to the other socket
            writeData outputSock msg

            -- log the message
            case t of
                Event -> STM.atomically $ T.writeTChan logger (ServerMessage msg)
                Request -> STM.atomically $ T.writeTChan logger (ClientMessage msg)

            loop om t inputSock outputSock logger

    return ()


    -- if the socket is no longer connected, end the thread
    -- M.unless (BS.null header) $ processMsg header

    where
        readData :: Socket.Socket -> ET.ErrorT String IO WMessage
        readData sock = do

            header <- ET.liftIO $ BSocket.recv sock 8

            ET.when (BS.null header) $ ET.throwError $ ET.strMsg "socket was closed"

            let p = parseHeader header

            let (objectId, opCode, msgSize) = case p of
                   (Left _, _) -> (0, 0, 0)
                   (Right (obj, op, m), _) -> (obj, op, m)
            ET.when (msgSize == 0) $ ET.throwError $ ET.strMsg "parsing error"

            let remaining = fromIntegral $ msgSize - 8

            -- get the XML model based on the opcode

            -- TODO: do not make these exceptions fatal

            mInterface <- ET.liftIO $ lookupMapping om (fromIntegral objectId)
            ET.when (Maybe.isNothing mInterface) $ ET.throwError $ ET.strMsg "unknown objectId"

            let mMessage = IM.lookup (fromIntegral opCode) (getMap t $ Maybe.fromJust mInterface)
            ET.when (Maybe.isNothing mMessage) $ ET.throwError $ ET.strMsg "unknown opcode"

            -- read data in blocks so that we don't accidentally go over fd
            let h = WHeader objectId msgSize opCode

            wmsg <- ET.liftIO $ readFullMessage inputSock h (Maybe.fromJust mMessage) header remaining

            -- input <- ET.liftIO $ BSocket.recv inputSock remaining
            -- ET.when (BS.null input) $ ET.throwError $ ET.strMsg "socket was closed"

            {-
            ET.liftIO $ putStrLn $ "message header: id=" ++ show objectId ++ ", size=" ++ show msgSize ++ ", opcode=" ++ show opCode
            ET.liftIO $ putStrLn "message content:"
            ET.liftIO $ dumpByteString input
            -}

            -- let msgData = BS.append header input
            -- let wmsg = WMessage h (parseMessage undefined input) msgData

            return wmsg


        writeData :: Socket.Socket -> WMessage -> IO ()
        writeData sock (WMessage header blocks bs) = do

            -- go through the blocks for FD passing

            s <- BSocket.send sock bs

            CC.threadDelay 100000

            return ()

        getMap :: MessageType -> WInterfaceDescription -> WMessageMap
        getMap messageType interface = case messageType of
            Request -> interfaceRequests interface
            Event -> interfaceEvents interface


clientThread :: STM.TMVar Event -> CC.MVar ObjectMap -> Socket.Socket -> Socket.Socket -> STM.TChan Message -> IO ()
clientThread eventV om clientSock serverSock loggerChan = do
    loop om Request clientSock serverSock loggerChan
    STM.atomically $ STM.putTMVar eventV ClientClosedSocket


serverThread :: STM.TMVar Event -> CC.MVar ObjectMap -> Socket.Socket -> Socket.Socket -> STM.TChan Message -> IO ()
serverThread eventV om serverSock clientSock loggerChan = do
    loop om Event serverSock clientSock loggerChan
    STM.atomically $ STM.putTMVar eventV ServerClosedSocket


execProcess :: FilePath -> [String] -> Socket.Socket -> IO a
execProcess path args sock = do
    let fd = show $ Socket.fdSocket sock

    env <- E.getEnvironment
    let filteredEnv = filter (\x -> fst x /= "WAYLAND_SOCKET") env

    putStrLn $ "Exec " ++ path ++ " with WAYLAND_SOCKET=" ++ fd
    Process.executeFile path True args (Just $ ("WAYLAND_SOCKET", fd):filteredEnv)

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
            let m = foldr (\i -> DM.insert (interfaceDescrName i) i) (DM.empty) is
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

    objectMap <- CC.newMVar $ IM.insert 1 display IM.empty

    -- read the WAYLAND_DISPLAY environment variable

    loggerChan <- STM.newTChanIO
    eventV <- STM.newEmptyTMVarIO

    _ <- Signals.installHandler Signals.sigINT (Signals.Catch $ sigHandler Signals.sigINT eventV) Nothing
    _ <- Signals.installHandler Signals.sigCHLD (Signals.Catch $ sigHandler Signals.sigCHLD eventV) Nothing

    _ <- CC.forkIO $ loggerThread objectMap loggerChan

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

    _ <- CC.forkIO $ serverThread eventV objectMap serverSock clientSock loggerChan

    _ <- CC.forkIO $ clientThread eventV objectMap clientSock serverSock loggerChan

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

