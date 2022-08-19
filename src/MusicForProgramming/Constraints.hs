module MusicForProgramming.Constraints where

import Network.Wreq (Response, get)
import Qtility
import qualified RIO.ByteString.Lazy as LazyByteString
import qualified RIO.Directory as Directory
import System.IO (putStrLn)

class (Monad m) => MonadFileIO m where
  doesFileExistM :: FilePath -> m Bool
  writeByteStringToFileM :: FilePath -> LByteString -> m ()

instance MonadFileIO IO where
  doesFileExistM = Directory.doesFileExist
  writeByteStringToFileM = LazyByteString.writeFile

class (Monad m) => MonadTerminalIO m where
  putStrLnM :: String -> m ()

instance MonadTerminalIO IO where
  putStrLnM = putStrLn

class (Monad m) => MonadHttp m where
  httpGetM :: String -> m (Response LByteString)

instance MonadHttp IO where
  httpGetM = get
