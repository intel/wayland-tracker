import qualified System.Console.CmdTheLine as Cmd
import Control.Applicative

import Tracker

xmlFiles :: Cmd.Term [String]
xmlFiles = Cmd.nonEmpty $ Cmd.optAll [] $ Cmd.optInfo ["xml-file", "x"]

logType :: Cmd.Term String
logType = Cmd.value $ Cmd.opt "simple" $ Cmd.optInfo ["output-type", "t"]

logFile :: Cmd.Term (Maybe String)
logFile = Cmd.value $ Cmd.opt Nothing $ Cmd.optInfo ["output-file", "o"]

command :: Cmd.Term String
command = Cmd.required $ Cmd.pos 0 Nothing Cmd.posInfo { Cmd.posName = "COMMAND" }

commandArgs :: Cmd.Term [String]
commandArgs = Cmd.value $ Cmd.posRight 0 [] Cmd.posInfo { Cmd.posName = "ARGS" }

term :: Cmd.Term (IO ())
term = program <$> xmlFiles <*> logType <*> logFile <*> command <*> commandArgs

program :: [String] -> String -> Maybe String -> String -> [String] -> IO ()
program xfs lt lf cmd cmdargs = runApplication xfs lt lf cmd cmdargs

termInfo :: Cmd.TermInfo
termInfo = Cmd.defTI { Cmd.termName = "wayland-tracker", Cmd.version = "0.1" }

main :: IO ()
main = Cmd.run (term, termInfo)
