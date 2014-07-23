{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Wayland (sendToWayland, recvFromWayland)

where

import Foreign
import Foreign.C.Types
import qualified Data.ByteString as BS
import qualified Network.Socket as Socket
-- import qualified Data.ByteString.Unsafe as UBS
import qualified Data.ByteString.Char8 as BSC

foreign import ccall unsafe "wayland-msg-handling.h sendmsg_wayland"
    c_sendmsg_wayland  :: CInt -- fd
        -> Ptr CChar -- buf
        -> CInt -- bufsize
        -> Ptr CInt -- fds
        -> CInt -- n_fds
        -> IO (Int) -- bytes sent

foreign import ccall unsafe "wayland-msg-handling.h recvmsg_wayland"
    c_recvmsg_wayland :: CInt -- fd
        -> Ptr CChar -- buf
        -> CInt -- bufsize
        -> Ptr CInt -- fds
        -> CInt -- fdbufsize
        -> Ptr CInt -- n_fds
        -> IO (Int) -- bytes received


sendToWayland :: Socket.Socket -> BS.ByteString -> [Int] -> IO Int
sendToWayland s bs fds = BS.useAsCStringLen bs sendData
    where
        socket = Socket.fdSocket s
        c_fds = map fromIntegral fds
        sendData (bytePtr, byteLen) = withArrayLen c_fds $ \fdLen fdArray -> do
            let c_byteLen = fromIntegral byteLen
            let c_fdLen = fromIntegral fdLen
            c_sendmsg_wayland socket bytePtr c_byteLen fdArray c_fdLen


recvFromWayland :: Socket.Socket -> IO (BS.ByteString, [Int])
recvFromWayland s = allocaArray 4096 $ \cbuf -> do
    alloca $ \nFds_ptr ->
        allocaArray 28 $ \fdArray -> do
            len <- c_recvmsg_wayland socket cbuf 4096 fdArray 28 nFds_ptr
            bs <- BS.packCStringLen (cbuf, len)
            nFds <- peek nFds_ptr
            fds <- peekArray (fromIntegral nFds) fdArray
            return (bs, (map fromIntegral fds))
    where
        socket = Socket.fdSocket s

main = do
    (sock1, sock2) <- Socket.socketPair Socket.AF_UNIX Socket.Stream Socket.defaultProtocol

    let sendBs = BSC.pack "foobar"

    sent <- sendToWayland sock1 sendBs []

    putStrLn $ "Sent bytes: " ++ show sent

    (recvBs, fds) <- recvFromWayland sock2

    putStrLn $ "Received: " ++ BSC.unpack recvBs


    -- sendToWayland sock1 BS.em
