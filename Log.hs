module Log (writeLog)

where

import qualified Data.ByteString as BS
import qualified Data.Aeson as A

import Types

writeLog :: Logger -> Message -> BS.ByteString -> IO ()
writeLog logger msg bs = undefined