module ParseWaylandXML (
    parseWaylandXML,
    WArgumentType(..),
    WArgumentDescription(..),
    WMessageDescription(..),
    WInterfaceDescription(..),
    WMessageMap)

where

import qualified Text.XML.Light as XML
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map.Strict as DM

data WArgumentType = WInt | WUint | WFixed | WString | WObject | WNewId | WArray | WFd deriving (Eq, Show)

data WArgumentDescription = WArgumentDescription {
    argDescrName :: String,
    argDescrType :: WArgumentType
} deriving (Eq, Show)

data WMessageDescription = WMessageDescription {
    msgDescrName :: String,
    msgDescrArgs :: [WArgumentDescription]
} deriving (Eq, Show)

data WInterfaceDescription = WInterfaceDescription {
    interfaceDescrName :: String,
    interfaceRequests :: WMessageMap,
    interfaceEvents :: WMessageMap
} deriving (Eq, Show)

type WMessageMap = IM.IntMap WMessageDescription

argumentMap :: DM.Map String WArgumentType
argumentMap = DM.fromList [ ("int", WInt), ("uint", WUint),
                            ("fixed", WFixed), ("string", WString),
                            ("object", WObject), ("new_id", WNewId),
                            ("array", WArray), ("fd", WFd) ]


parseArgument :: XML.Element -> Maybe WArgumentDescription
parseArgument e = do
    argName <- XML.findAttr (XML.QName "name" Nothing Nothing) e
    argTypeString <- XML.findAttr (XML.QName "type" Nothing Nothing) e
    argType <- DM.lookup argTypeString argumentMap
    return $ WArgumentDescription argName argType


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
        messageMap = IM.fromList . L.zip [1..]


parseWaylandXML :: String -> Maybe [WInterfaceDescription]
parseWaylandXML fileData = do
    xmlDoc <- XML.parseXMLDoc fileData
    mapM parseInterface $ xmlInterfaces xmlDoc

    where
        xmlInterfaces :: XML.Element -> [XML.Element]
        xmlInterfaces = XML.findElements (XML.QName "interface" Nothing Nothing)
