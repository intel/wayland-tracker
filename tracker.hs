module Main where

import qualified Control.Monad as M
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified System.IO.Error as Err
import qualified System.Environment as E
import qualified System.Posix.Env as PE
import qualified System.Posix.User as PU
import qualified System.Posix.IO as PI
import qualified System.Posix.Process as Process
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as PT
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as BSocket
import qualified System.IO as SIO
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as T

data Message = ClientMessage | ServerMessage

loggerThread :: STM.TChan Message -> IO ()
loggerThread chan =

    M.forever $ processData chan

    where
        processData input = do
            x <- STM.atomically $ STM.readTChan input
            case x of
                ClientMessage -> putStrLn "read a client message"
                ServerMessage -> putStrLn "read a server message"

signalThread :: STM.TMVar Signals.SignalSet -> IO ()
signalThread sig = do

    let signals = foldr Signals.addSignal Signals.emptySignalSet [Signals.sigINT, Signals.sigCHLD]

    Signals.awaitSignal $ Just signals

    pending <- Signals.getPendingSignals

    -- send the signal to the master thread
    STM.atomically $ STM.putTMVar sig pending


parseClientMessage bs = ClientMessage

parseServerMessage bs = ServerMessage

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
            -- x <- BSocket.send outputSock input

            -- parse data

            let msg = f input

            -- send parsed data to logger

            STM.atomically $ T.writeTChan logger msg

            loop f inputSock outputSock logger


clientThread :: Socket.Socket -> Socket.Socket -> STM.TChan Message -> IO ()
clientThread =
    loop parseClientMessage

serverThread :: Socket.Socket -> Socket.Socket -> STM.TChan Message -> IO ()
serverThread = loop parseServerMessage

execProcess :: FilePath -> [String] -> Socket.Socket -> IO a
execProcess path args sock = do
    let fd = show $ Socket.fdSocket sock

    putStrLn $ "Exec " ++ path ++ " with WAYLAND_SOCKET=" ++ fd
    Process.executeFile path True args (Just [("WAYLAND_SOCKET", fd)])

createXdgPath :: a -> IO String
createXdgPath _ = do
    userid <- PU.getRealUserID
    return $ "/var/run/" ++ show userid

main :: IO ()
main = do
    -- read the WAYLAND_DISPLAY environment variable
    --
    loggerChan <- STM.newTChanIO
    signalV <- STM.newEmptyTMVarIO

    lt <- CC.forkIO $ loggerThread loggerChan
    st <- CC.forkIO $ signalThread signalV

    xdgDir <- Err.catchIOError (E.getEnv "XDG_RUNTIME_DIR") createXdgPath
    serverName <- Err.catchIOError (E.getEnv "WAYLAND_DISPLAY") (\_ -> return "wayland-0")

    -- open the connection to the server

    serverSock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    PI.setFdOption (PT.Fd $ Socket.fdSocket serverSock) PI.CloseOnExec True

    let serverPath = xdgDir ++ "/" ++ serverName

    putStrLn $ "Connecting to " ++ serverPath

    -- Socket.connect serverSock (Socket.SockAddrUnix serverPath)

    -- create socket for the child and start a thread for it

    (clientSock, trackerSock) <- Socket.socketPair Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    PI.setFdOption (PT.Fd $ Socket.fdSocket clientSock) PI.CloseOnExec True

    -- start threads for communication

    -- st <- CC.forkIO $ serverThread serverSock clientSock loggerChan

    ct <- CC.forkIO $ clientThread clientSock serverSock loggerChan

    -- fork the child

    pid <- Process.forkProcess $ execProcess "weston-terminal" [] trackerSock

    Socket.close trackerSock

    -- process messages until the child dies (closes the socket), server dies or
    -- there is a SIGINT

    s <- STM.atomically $ do
        sig <- STM.takeTMVar signalV
        return sig

    putStrLn $ "received signal: " ++ show s

    M.forever $ putStr ""

    -- in case of SIGINT, send the same signal to the child and terminate

    Signals.signalProcess Signals.sigINT pid

