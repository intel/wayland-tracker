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

binaryMode = BinaryMode
        {
            output = def &= typFile &= help "Output file",
            command = def &= argPos 0 &= typ "PROGRAM",
            commandArgs = def &= args
        } &= name "binary"

jsonMode = JsonMode
        {
            xmlFile = def &= typFile &= help "Protocol description XML file",
            output = def &= typFile &= help "Output file",
            command = def &= argPos 0 &= typ "PROGRAM",
            commandArgs = def &= args
        } &= name "json"

jsonPrettyMode = JsonPrettyMode
        {
            xmlFile = def &= typFile &= help "Protocol description XML file",
            output = def &= typFile &= help "Output file",
            command = def &= argPos 0 &= typ "PROGRAM",
            commandArgs = def &= args &= typ "PROGRAM OPTIONS"
        } &= name "json_pretty"

main = do
    let m = modes [binaryMode &= auto, jsonMode, jsonPrettyMode]
            &= program "wayland-tracker"
            &= summary "Wayland protocol message dumper, version 0.1"
            &= helpArg [name "h"]
    args <- cmdArgsRun (cmdArgsMode $ m)
    case args of
        BinaryMode o c cargs -> runApplication [] Binary o c cargs
        JsonMode xs o c cargs -> runApplication xs Json o c cargs
        JsonPrettyMode xs o c cargs -> runApplication xs JsonPretty o c cargs
