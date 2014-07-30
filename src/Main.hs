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

{-# LANGUAGE DeriveDataTypeable #-}

module Main

where

import System.Console.CmdArgs

import Types
import Tracker

data OutputMode = BinaryMode {
                    output :: Maybe String,
                    command :: String,
                    commandArgs :: [String]
                }
                | JsonMode {
                    xmlFile :: [String],
                    output :: Maybe String,
                    command :: String,
                    commandArgs :: [String]
                }
                | JsonPrettyMode {
                    xmlFile :: [String],
                    output :: Maybe String,
                    command :: String,
                    commandArgs :: [String]
                } deriving (Show, Data, Typeable)

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
