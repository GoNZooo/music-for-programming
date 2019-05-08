{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative             (many, (<|>))
import qualified Control.Exception.Safe          as E
import           Control.Lens
import           Control.Monad                   (when)
import           Data.ByteString.Lazy            (ByteString)
import qualified Data.ByteString.Lazy            as LBS
import           Data.Maybe                      (isNothing)
import           Data.Text                       (Text, pack, replace, toLower,
                                                  unpack)
import           Network.HTTP.Client             (HttpException (..),
                                                  HttpExceptionContent (..))
import           Network.Wreq                    (Response, get, responseBody,
                                                  responseStatus, statusCode)
import           System.Directory                (doesFileExist)
import           System.Exit                     (exitFailure)
import           Text.HTML.Scalpel               (Scraper, chroot, scrapeURL,
                                                  tagSelector, texts, (@:),
                                                  (@=))
import           Util                            (split)

import           MusicForProgramming.Config      (downloadPath, getConfig,
                                                  getConfigDirectory)
import           MusicForProgramming.Constraints (MonadFileIO, MonadHttp,
                                                  MonadTerminalIO,
                                                  doesFileExistM, httpGetM,
                                                  putStrLnM,
                                                  writeByteStringToFileM)

newtype FileName =
  FileName String
  deriving (Show)

newtype Link =
  Link String
  deriving (Show)

main :: IO ()
main = do
  configDirectory <- getConfigDirectory
  config <- getConfig
  when
    (isNothing config)
    (putStrLn
       ("Config not available or unreadable @ '" <> configDirectory <> "'") >>
     exitFailure)
  let Just cfg = config
  result <- scrapeURL "http://musicforprogramming.net/" mp3Links
  case result of
    Just mp3s -> do
      results <-
        maybe (pure []) (traverse (downloadIfNotExists (downloadPath cfg))) mp3s
      pure ()
    Nothing -> putStrLn "Couldn't scrape page for download links."

downloadIfNotExists ::
     (Monad m, MonadFileIO m, MonadTerminalIO m, MonadHttp m, E.MonadCatch m)
  => FilePath
  -> Link
  -> m (Int, FilePath)
downloadIfNotExists baseDir l = do
  exists <- doesFileExistM (fileName baseDir l)
  if exists
    then do
      putStrLnM $ fileName baseDir l <> " already downloaded."
      pure (200, fileName baseDir l)
    else do
      putStrLnM $ "Downloading to " <> fileName baseDir l
      downloadFile l >>= writeResponseToFile (fileName baseDir l)

downloadFile ::
     (Monad m, MonadHttp m, MonadFileIO m, MonadTerminalIO m, E.MonadCatch m)
  => Link
  -> m (Response ByteString)
downloadFile (Link l) =
  httpGetM l `E.catch`
  (\(HttpExceptionRequest req (StatusCodeException resp bs)) -> do
     case resp ^. responseStatus . statusCode of
       404 -> putStrLnM $ "ERROR: File not found (404) for " <> l
       code ->
         putStrLnM $ "ERROR: Unknown error with code " <> show code <> " for " <>
         l
     pure $ fmap (const (LBS.fromStrict bs)) resp)

writeResponseToFile ::
     (Monad m, MonadFileIO m)
  => FilePath
  -> Response ByteString
  -> m (Int, FilePath)
writeResponseToFile path response = do
  let responseCode = response ^. responseStatus . statusCode
  when
    (responseCode == 200)
    (writeByteStringToFileM path (response ^. responseBody))
  pure (responseCode, path)

mp3Links :: Scraper String (Maybe [Link])
mp3Links = chroot ("div" @: ["id" @= "episodes"]) mp3Elements

mp3Elements :: Scraper String (Maybe [Link])
mp3Elements = do
  anchorContents <- texts (tagSelector "a")
  spanContents <- texts (tagSelector "span")
  case traverse makeLink (anchorContents ++ spanContents) of
    Just links -> pure . Just $ Link <$> links
    Nothing    -> pure Nothing

makeLink :: String -> Maybe String
makeLink (d1:d2:_:_:compiler) =
  Just (mconcat [makePrefix d1 d2, sanitizeCompiler compiler, ".mp3"])
makeLink _ = Nothing

makePrefix :: Char -> Char -> String
makePrefix '0' d =
  mconcat ["http://datashat.net/music_for_programming_", [d], "-"]
makePrefix d1 d2 =
  mconcat ["http://datashat.net/music_for_programming_", [d1, d2], "-"]

sanitizeCompiler :: String -> String
sanitizeCompiler =
  unpack . toLower . replaceDoubleUnderscores . replaceDots . replaceSpace .
  replacePlus .
  pack

replacePlus :: Text -> Text
replacePlus = replace "+" "and"

replaceSpace :: Text -> Text
replaceSpace = replace " " "_"

replaceDots :: Text -> Text
replaceDots = replace "." "_"

replaceDoubleUnderscores :: Text -> Text
replaceDoubleUnderscores = replace "__" "_"

(</>) :: FilePath -> FilePath -> FilePath
fp1 </> fp2 = fp1 <> "/" <> fp2

fileName :: FilePath -> Link -> FilePath
fileName baseDir (Link l) = baseDir </> baseName
  where
    baseName = last pathComponents
    pathComponents = split '/' l
