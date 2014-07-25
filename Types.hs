{-# LANGUAGE OverloadedStrings #-}

module Types (
    MessageType(..),
    MArgumentValue(..),
    MArgument(..),
    ParsedMessage(..),
    ParsedBinaryMessage(..),
    LogType(..),
    Logger(..))

where

import qualified Data.ByteString as BS
import qualified Data.Aeson as A
import qualified System.IO as IO
import qualified Data.ByteString.Base16 as B16
import qualified Numeric as N

data MessageType = Request | Event deriving (Eq, Show)

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

data ParsedMessage = UnknownMessage
                   | Message {
                         msgType :: MessageType,
                         msgName :: String,
                         msgInterface :: String,
                         msgArguments :: [MArgument]
                     } deriving (Eq, Show)

data ParsedBinaryMessage = ParsedBinaryMessage {
                                binaryMsgType :: MessageType,
                                senderId :: Int,
                                opCode :: Int,
                                msgSize :: Int,
                                msgData :: BS.ByteString
                           } deriving (Eq, Show)

data LogType = Simple | Binary | Json

data Logger = Logger IO.Handle LogType

-- Helper to convert a Wayland Fixed number to Double
fixedToFloat :: MArgumentValue -> Double
fixedToFloat (MFixed sign fp sp) = signed sign (head values)
    where
        values = N.readFloat $ (show fp) ++ "." ++ (show sp)
        signed s (float, _) = if s
            then float
            else -1.0 * float

instance A.ToJSON MArgumentValue where
    toJSON value = case value of
        MInt v -> A.object [ "type" A..= A.String "Int", "value" A..= v ]
        MUInt v -> A.object [ "type" A..= A.String "UInt", "value" A..= v ]
        MString v -> A.object [ "type" A..= A.String "UInt", "value" A..= v ]
        MFixed _ _ _ -> A.object [ "type" A..= A.String "Fixed",
                                        "value" A..= fixedToFloat value ]
        MArray bs -> A.object [ "type" A..= A.String "Array", "value" A..= B16.encode bs ]
        MFd -> A.object [ "type" A..= A.String "Fd" ]
        MNewId v _ -> A.object [ "type" A..= A.String "NewId", "value" A..= v ]
        MObject v -> A.object [ "type" A..= A.String "Object", "value" A..= v ]

instance A.ToJSON MArgument where
    toJSON (MArgument name value) = A.object [ "name" A..= name, "value" A..= value ]

instance A.ToJSON MessageType where
    toJSON Request = A.String "Request"
    toJSON Event = A.String "Event"

instance A.ToJSON ParsedMessage where
    toJSON UnknownMessage = A.String "unknown message"
    toJSON (Message t n i as) = A.object
        [ "type" A..= t,
          "name" A..= n,
          "interface" A..= i,
          "arguments" A..= as ]
