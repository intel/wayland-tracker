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

module ParseWaylandXML (
    parseWaylandXML,
    WArgumentType(..),
    WArgumentDescription(..),
    WMessageDescription(..),
    WInterfaceDescription(..),
    WMessageMap)

where

import qualified Data.IntMap     as IM
import qualified Data.List       as L
import qualified Data.Map.Strict as DM
import qualified Data.Maybe      as Maybe
import qualified Text.XML.Light  as XML

data WArgumentType = WInt
                   | WUInt
                   | WFixed
                   | WString
                   | WObject
                   | WNewId
                   | WArray
                   | WFd deriving (Eq, Show)

data WArgumentDescription = WArgumentDescription
    { argDescrName      :: String,
      argDescrType      :: WArgumentType,
      argDescrInterface :: String
    } deriving (Eq, Show)

data WMessageDescription = WMessageDescription
    { msgDescrName :: String,
      msgDescrArgs :: [WArgumentDescription]
    } deriving (Eq, Show)

data WInterfaceDescription = WInterfaceDescription
    { interfaceDescrName :: String,
      interfaceRequests  :: WMessageMap,
      interfaceEvents    :: WMessageMap
    } deriving (Eq, Show)

type WMessageMap = IM.IntMap WMessageDescription

argumentMap :: DM.Map String WArgumentType
argumentMap = DM.fromList [ ("int", WInt), ("uint", WUInt),
                            ("fixed", WFixed), ("string", WString),
                            ("object", WObject), ("new_id", WNewId),
                            ("array", WArray), ("fd", WFd) ]


parseArgument :: XML.Element -> Maybe WArgumentDescription
parseArgument e = do
    argName <- XML.findAttr (XML.QName "name" Nothing Nothing) e
    argTypeString <- XML.findAttr (XML.QName "type" Nothing Nothing) e
    argType <- DM.lookup argTypeString argumentMap
    let argInterface = Maybe.fromMaybe "" (XML.findAttr (XML.QName "interface" Nothing Nothing) e)
    return $ WArgumentDescription argName argType argInterface


parseMessage :: XML.Element -> Maybe WMessageDescription
parseMessage e = do
    messageName <- XML.findAttr (XML.QName "name" Nothing Nothing) e
    arguments <- mapM parseArgument $ xmlArguments e
    return $ WMessageDescription messageName arguments

    where
        xmlArguments :: XML.Element -> [XML.Element]
        xmlArguments = XML.findElements (XML.QName "arg" Nothing Nothing)


parseInterface :: XML.Element -> Maybe WInterfaceDescription
parseInterface e = do
    interfaceName <- XML.findAttr (XML.QName "name" Nothing Nothing) e
    requests <- mapM parseMessage $ xmlRequests e
    events <- mapM parseMessage $ xmlEvents e
    return $ WInterfaceDescription interfaceName (messageMap requests) (messageMap events)

    where
        xmlRequests :: XML.Element -> [XML.Element]
        xmlRequests = XML.findElements (XML.QName "request" Nothing Nothing)

        xmlEvents :: XML.Element -> [XML.Element]
        xmlEvents = XML.findElements (XML.QName "event" Nothing Nothing)

        messageMap :: [WMessageDescription] -> WMessageMap
        messageMap = IM.fromList . L.zip [0..] -- opcodes start from 0


parseWaylandXML :: String -> Maybe [WInterfaceDescription]
parseWaylandXML fileData = do
    xmlDoc <- XML.parseXMLDoc fileData
    mapM parseInterface $ xmlInterfaces xmlDoc

    where
        xmlInterfaces :: XML.Element -> [XML.Element]
        xmlInterfaces = XML.findElements (XML.QName "interface" Nothing Nothing)
