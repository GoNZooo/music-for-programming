{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception.Safe as E
import Data.Maybe (fromJust)
import Data.Text (pack, replace, toLower, unpack)
import MusicForProgramming.Config (downloadPath, getConfig, getConfigDirectory)
import MusicForProgramming.Constraints
  ( MonadFileIO,
    MonadHttp,
    MonadTerminalIO,
    doesFileExistM,
    httpGetM,
    putStrLnM,
    writeByteStringToFileM,
  )
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq (Response, responseBody, responseStatus, statusCode)
import Qtility
import qualified RIO.List.Partial as PartialList
import Text.HTML.Scalpel (Scraper, chroot, scrapeURL, tagSelector, texts, (@:), (@=))
import Util (split)

newtype FileName
  = FileName String
  deriving (Show)

newtype Link
  = Link String
  deriving (Show)

main :: IO ()
main = do
  configDirectory <- getConfigDirectory
  config <- getConfig
  when (isNothing config) $ do
    putStrLnM ("Config not available or unreadable @ '" <> configDirectory <> "'") >> exitFailure
  let cfg = fromJust config
  result <- scrapeURL "http://musicforprogramming.net/" mp3Links
  let downloadAllLinks = mapM_ $ downloadIfNotExists (downloadPath cfg)
  case result of
    Just mp3s -> mapM_ downloadAllLinks mp3s
    Nothing -> putStrLnM "Couldn't scrape page for download links."

downloadIfNotExists ::
  (MonadFileIO m, MonadTerminalIO m, MonadHttp m, E.MonadCatch m) =>
  FilePath ->
  Link ->
  m (Maybe (Int, FilePath))
downloadIfNotExists baseDir l = do
  exists <- doesFileExistM (fileName baseDir l)
  if exists
    then do
      putStrLnM $ fileName baseDir l <> " already downloaded."
      pure $ Just (200, fileName baseDir l)
    else do
      putStrLnM $ "Downloading to " <> fileName baseDir l
      maybeResponse <- downloadFile l
      case maybeResponse of
        Just response ->
          Just <$> writeResponseToFile (fileName baseDir l) response
        Nothing ->
          pure Nothing

downloadFile ::
  (MonadHttp m, MonadTerminalIO m, E.MonadCatch m) =>
  Link ->
  m (Maybe (Response LByteString))
downloadFile link'@(Link l) =
  (Just <$> httpGetM l) `E.catch` handleHttpError link'

handleHttpError :: (MonadTerminalIO m) => Link -> HttpException -> m (Maybe (Response LByteString))
handleHttpError (Link link') (HttpExceptionRequest _req (StatusCodeException resp _bytestring)) = do
  case resp ^. responseStatus . statusCode of
    404 -> putStrLnM $ "ERROR: File not found (404) for " <> link'
    code ->
      putStrLnM $ "ERROR: Unknown HTTP error with code " <> show code <> " for " <> link'
  pure Nothing
handleHttpError _ _ = pure Nothing

writeResponseToFile :: (MonadFileIO m) => FilePath -> Response LByteString -> m (Int, FilePath)
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
    Nothing -> pure Nothing

makeLink :: String -> Maybe String
makeLink (d1 : d2 : _ : _ : compiler) =
  Just (mconcat [makePrefix d1 d2, sanitizeCompiler compiler, ".mp3"])
makeLink _ = Nothing

makePrefix :: Char -> Char -> String
makePrefix '0' d =
  mconcat ["http://datashat.net/music_for_programming_", [d], "-"]
makePrefix d1 d2 =
  mconcat ["http://datashat.net/music_for_programming_", [d1, d2], "-"]

sanitizeCompiler :: String -> String
sanitizeCompiler =
  unpack . toLower . replaceDoubleUnderscores . replaceDots . replaceSpace . replacePlus . pack

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
    baseName = PartialList.last pathComponents
    pathComponents = split '/' l
