{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Types (
    MessageType(..),
    MArgumentValue(..),
    MArgument(..),
    ParsedMessage(..),
    LogType(..))

where

import qualified Data.ByteString as BS
import qualified System.IO as IO
import GHC.Generics

data MessageType = Request | Event deriving (Eq, Show)

data MArgumentValue = MInt Int
                    | MUInt Int
                    | MString String
                    | MFixed Bool Int Int
                    | MArray BS.ByteString
                    | MFd
                    | MNewId Int String
                    | MObject Int
                           deriving (Eq, Show, Generic)

data MArgument = MArgument {
                     argName :: String,
                     argValue :: MArgumentValue
                 } deriving (Eq, Show, Generic)

data ParsedMessage = UnknownMessage
                   | Message {
                         msgType :: MessageType,
                         msgName :: String,
                         msgInterface :: String,
                         msgArguments :: [MArgument]
                     } deriving (Eq, Show,  Generic)


data LogType = Simple | Binary | Json


data Logger = Logger IO.Handle LogType