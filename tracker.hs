module Main where

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
import qualified Text.XML.Light as X
import qualified Data.Maybe as Maybe

import ParseWaylandXML

-- messages contain header and a list of data / fd blocks

data WHeader = WHeader { object :: W.Word32, size :: W.Word16, opcode :: W.Word16 }

data WMessageBlockType = WFD | WInt | WUInt | WFixed | WObject | WNewId | WString | WArray

data WMessageBlock = WMessageBlock {
    start :: Int,
    blockLength :: Int,
    blockType :: WMessageBlockType
}

type WXmlModel = [WMessageBlockType]

type WMessageModel = [WMessageBlock]

data WMessage = WMessage WHeader [WMessageBlock] BS.ByteString

data Message = ClientMessage WMessage | ServerMessage WMessage

data Event = ServerClosedSocket | ClientClosedSocket | SigChld | SigInt
    deriving (Show, Eq)

dumpByteString :: BS.ByteString -> IO ()
dumpByteString bs = do
        M.mapM_ (\n -> putStr $ N.showHex n " ") bytes
        putStrLn ""
    where
        bytes = BS.unpack bs

loggerThread :: STM.TChan Message -> IO ()
loggerThread chan =

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

parseClientMessage :: WMessage -> Message
parseClientMessage = ClientMessage

parseServerMessage :: WMessage -> Message
parseServerMessage = ServerMessage

parseMessage :: WXmlModel -> BS.ByteString -> [WMessageBlock]
parseMessage model bs = msgblocks
    where
        dataLength = BS.length bs
        msgblocks = [WMessageBlock 0 dataLength WString]


parseHeader :: BS.ByteString -> (Either String (W.Word32, W.Word16, W.Word16), BS.ByteString)
parseHeader = BG.runGet process
    where
        process :: BG.Get (W.Word32, W.Word16, W.Word16)
        process = do
            senderId <- BG.getWord32le
            msgSize <- BG.getWord16le
            msgOpcode <- BG.getWord16le
            return (senderId, msgSize, msgOpcode)

loop :: (WMessage -> Message) -> Socket.Socket -> Socket.Socket -> T.TChan Message -> IO ()
loop f inputSock outputSock logger =  do
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
            STM.atomically $ T.writeTChan logger (f msg)
            loop f inputSock outputSock logger

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

            -- TODO: get the XML model based on the opcode
            -- read data in blocks so that we don't accidentally go over fd

            input <- ET.liftIO $ BSocket.recv inputSock remaining
            ET.when (BS.null input) $ ET.throwError $ ET.strMsg "socket was closed"

            {-
            ET.liftIO $ putStrLn $ "message header: id=" ++ show objectId ++ ", size=" ++ show msgSize ++ ", opcode=" ++ show opCode
            ET.liftIO $ putStrLn "message content:"
            ET.liftIO $ dumpByteString input
            -}

            let h = WHeader objectId msgSize opCode
            let msgData = BS.append header input
            let wmsg = WMessage h (parseMessage undefined input) msgData

            return wmsg


        writeData :: Socket.Socket -> WMessage -> IO ()
        writeData sock (WMessage header blocks bs) = do

            -- go through the blocks for FD passing

            s <- BSocket.send sock bs

            CC.threadDelay 100000

            return ()


clientThread :: STM.TMVar Event -> Socket.Socket -> Socket.Socket -> STM.TChan Message -> IO ()
clientThread eventV clientSock serverSock loggerChan = do
    loop parseClientMessage clientSock serverSock loggerChan
    STM.atomically $ STM.putTMVar eventV ClientClosedSocket


serverThread :: STM.TMVar Event -> Socket.Socket -> Socket.Socket -> STM.TChan Message -> IO ()
serverThread eventV serverSock clientSock loggerChan = do
    loop parseServerMessage serverSock clientSock loggerChan
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

parseProtocol root = (events, requests)
    where
        events = undefined
        requests = undefined

main :: IO ()
main = do

    -- read the protocol file(s)

    xmlFile <- readFile "xml/wayland.xml"
    let xmlDoc = X.parseXMLDoc xmlFile

    M.when (Maybe.isNothing xmlDoc) $ do
        putStrLn "Error parsing the XML file"
        Exit.exitFailure

    let (events, requests) = parseProtocol $ Maybe.fromJust xmlDoc

    -- read the WAYLAND_DISPLAY environment variable

    loggerChan <- STM.newTChanIO
    eventV <- STM.newEmptyTMVarIO

    _ <- Signals.installHandler Signals.sigINT (Signals.Catch $ sigHandler Signals.sigINT eventV) Nothing
    _ <- Signals.installHandler Signals.sigCHLD (Signals.Catch $ sigHandler Signals.sigCHLD eventV) Nothing

    _ <- CC.forkIO $ loggerThread loggerChan

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

    pid <- Process.forkProcess $ execProcess "weston-terminal" [] trackerSock

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

