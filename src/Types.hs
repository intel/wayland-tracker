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
import qualified Data.ByteString.Char8 as C8
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

data LogType = Binary | Json

data Logger = Logger IO.Handle LogType

-- Helper to convert a Wayland Fixed number to Double
fixedToFloat :: MArgumentValue -> Double
fixedToFloat (MFixed sign fp sp) = signed sign (head values)
    where
        values = N.readFloat $ (show fp) ++ "." ++ (show sp)
        signed s (float, _) = if s
            then -1.0 * float
            else float

instance A.ToJSON MArgumentValue where
    toJSON value = case value of
        MInt v -> A.object [ "type" A..= A.String "Int", "value" A..= v ]
        MUInt v -> A.object [ "type" A..= A.String "UInt", "value" A..= v ]
        MString v -> A.object [ "type" A..= A.String "UInt", "value" A..= v ]
        MFixed _ _ _ -> A.object [ "type" A..= A.String "Fixed",
                                   "value" A..= fixedToFloat value ]
        MArray bs -> A.object [ "type" A..= A.String "Array",
                                "value" A..= (C8.unpack $ B16.encode bs) ]
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
