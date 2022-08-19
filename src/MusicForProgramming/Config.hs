{-# LANGUAGE OverloadedStrings #-}

module MusicForProgramming.Config
  ( getConfig,
    downloadPath,
    getConfigDirectory,
  )
where

import qualified Data.Yaml as Yaml
import Qtility
import RIO.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getXdgDirectory,
  )

newtype Config = Config {downloadPath :: FilePath}
  deriving (Show)

getConfigDirectory :: IO FilePath
getConfigDirectory = do
  path <- getXdgDirectory XdgConfig "music-for-programming"
  createDirectoryIfMissing True path -- True; create parents too
  pure path

getConfig :: IO (Maybe Config)
getConfig = do
  configDirectory <- getConfigDirectory
  let fileName = configDirectory <> "/config.yml"
  parsedConfig <- Yaml.decodeFileEither fileName
  either (const (pure Nothing)) (pure . Just) parsedConfig

instance FromJSON Config where
  parseJSON (Yaml.Object v) = Config <$> v .: "download-path"
  parseJSON _ = fail "Need an object to decode"
