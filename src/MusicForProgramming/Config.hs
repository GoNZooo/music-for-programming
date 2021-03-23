{-# LANGUAGE OverloadedStrings #-}

module MusicForProgramming.Config
  ( getConfig,
    downloadPath,
    getConfigDirectory,
  )
where

import Data.Yaml (FromJSON (..), (.:))
import qualified Data.Yaml as Y
import System.Directory
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
  parsedConfig <- Y.decodeFileEither fileName
  either (const (pure Nothing)) (pure . Just) parsedConfig

instance FromJSON Config where
  parseJSON (Y.Object v) = Config <$> v .: "download-path"
  parseJSON _ = fail "Need an object to decode"
