module Main where

import qualified Control.Monad as M
-- import qualified Data.Binary as B
import qualified Data.ByteString as BS
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

data Message = ClientMessage | ServerMessage

data Event = ServerClosedSocket | ClientClosedSocket | SigChld | SigInt
    deriving (Show, Eq)

loggerThread :: STM.TChan Message -> IO ()
loggerThread chan =

    M.forever $ processData chan

    where
        processData input = do
            x <- STM.atomically $ STM.readTChan input
            case x of
                ClientMessage -> putStrLn "read a client message"
                ServerMessage -> putStrLn "read a server message"

parseClientMessage :: BS.ByteString -> Message
parseClientMessage _ = ClientMessage

parseServerMessage :: BS.ByteString -> Message
parseServerMessage _ = ServerMessage

loop :: (BS.ByteString -> Message) -> Socket.Socket -> Socket.Socket -> T.TChan Message -> IO ()
loop f inputSock outputSock logger =  do
    -- read from client socket
    putStrLn "recv from socket"
    input <- BSocket.recv inputSock 4096
    putStrLn "returned from recv"
    -- if the socket is no longer connected, end the thread

    M.unless (BS.null input) $ processData input

    where
        processData input = do
            putStrLn "processing input"
            _ <- BSocket.send outputSock input

            -- parse data

            let msg = f input

            -- send parsed data to logger

            STM.atomically $ T.writeTChan logger msg

            loop f inputSock outputSock logger


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

main :: IO ()
main = do
    -- read the WAYLAND_DISPLAY environment variable
    --
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
            SigInt -> putStrLn "sigINT received"
            SigChld -> putStrLn "sigCHLD received"
            ServerClosedSocket -> do
                putStrLn "server closed socket"
                Signals.signalProcess Signals.sigINT pid
            ClientClosedSocket -> putStrLn "client closed socket"

        -- all these should cause us to exit
        putStrLn "Exiting wayland-tracker"
        Exit.exitSuccess

