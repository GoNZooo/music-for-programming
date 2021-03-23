module MusicForProgramming.Constraints where

import qualified Data.ByteString.Lazy as LBS
import Network.Wreq (Response, get)
import qualified System.Directory as Directory

class (Monad m) => MonadFileIO m where
  doesFileExistM :: FilePath -> m Bool
  writeByteStringToFileM :: FilePath -> LBS.ByteString -> m ()

instance MonadFileIO IO where
  doesFileExistM = Directory.doesFileExist
  writeByteStringToFileM = LBS.writeFile

class (Monad m) => MonadTerminalIO m where
  putStrLnM :: String -> m ()

instance MonadTerminalIO IO where
  putStrLnM = putStrLn

class (Monad m) => MonadHttp m where
  httpGetM :: String -> m (Response LBS.ByteString)

instance MonadHttp IO where
  httpGetM = get
