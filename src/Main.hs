{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs

import Types
import Tracker

data OutputMode = BinaryMode
                    {
                        output :: Maybe String,
                        command :: String,
                        commandArgs :: [String]
                    }
                 | JsonMode
                    {
                        xmlFile :: [String],
                        output :: Maybe String,
                        command :: String,
                        commandArgs :: [String]
                    }
                 | JsonPrettyMode
                    {
                        xmlFile :: [String],
                        output :: Maybe String,
                        command :: String,
                        commandArgs :: [String]
                    }
                        deriving (Show, Data, Typeable)

binaryMode :: OutputMode
binaryMode = BinaryMode
        {
            output = def &= typFile &= help "Output file",
            command = def &= argPos 0 &= typ "PROGRAM",
            commandArgs = def &= args
        } &= name "binary"

jsonMode :: OutputMode
jsonMode = JsonMode
        {
            xmlFile = def &= typFile &= help "Protocol description XML file",
            output = def &= typFile &= help "Output file",
            command = def &= argPos 0 &= typ "PROGRAM",
            commandArgs = def &= args
        } &= name "json"

jsonPrettyMode :: OutputMode
jsonPrettyMode = JsonPrettyMode
        {
            xmlFile = def &= typFile &= help "Protocol description XML file",
            output = def &= typFile &= help "Output file",
            command = def &= argPos 0 &= typ "PROGRAM",
            commandArgs = def &= args &= typ "PROGRAM OPTIONS"
        } &= name "json_pretty"

main :: IO ()
main = do
    let m = modes [binaryMode &= auto, jsonMode, jsonPrettyMode]
            &= program "wayland-tracker"
            &= summary "Wayland protocol message dumper, version 0.1"
            &= helpArg [name "h"]
    parsedArgs <- cmdArgsRun (cmdArgsMode m)
    case parsedArgs of
        BinaryMode o c cargs -> runApplication [] Binary o c cargs
        JsonMode xs o c cargs -> runApplication xs Json o c cargs
        JsonPrettyMode xs o c cargs -> runApplication xs JsonPretty o c cargs
