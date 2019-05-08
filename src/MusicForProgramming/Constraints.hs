module MusicForProgramming.Constraints where

import qualified Data.ByteString.Lazy as LBS
import           Network.Wreq         (Response, get, responseBody,
                                       responseStatus, statusCode)
import qualified System.Directory     as Directory

class MonadFileIO m where
  doesFileExistM :: FilePath -> m Bool
  writeByteStringToFileM :: FilePath -> LBS.ByteString -> m ()

instance MonadFileIO IO where
  doesFileExistM = Directory.doesFileExist
  writeByteStringToFileM = LBS.writeFile

class MonadTerminalIO m where
  putStrLnM :: String -> m ()

instance MonadTerminalIO IO where
  putStrLnM = putStrLn

class MonadHttp m where
  httpGetM :: String -> m (Response LBS.ByteString)

instance MonadHttp IO where
  httpGetM = get
