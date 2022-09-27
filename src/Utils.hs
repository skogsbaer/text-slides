module Utils where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Crypto.Hash.MD5 as MD5
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Development.Shake
import Logging
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf

withTiming :: MonadIO m => m a -> m (Double, a)
withTiming action = do
  t0 <- liftIO getCurrentTime
  res <- action
  t1 <- liftIO getCurrentTime
  let delta = t1 `diffUTCTime` t0
  return (realToFrac delta, res)

withDevNull :: (MonadFail m, MonadIO m) => (Handle -> m a) -> m a
withDevNull action = do
  -- don't care about the fact that h is left open of action fails...
  h <- do
    h1 <- liftIO $ try $ openFile "/dev/null" WriteMode
    case h1 of
      Right x -> return x
      Left (_ :: SomeException) -> do
        h2 <- liftIO $ try $ openFile "NUL" WriteMode
        case h2 of
          Left (_ :: SomeException) -> fail "Cannot open /dev/null or NUL"
          Right x -> return x
  res <- action h
  liftIO $ hClose h
  return res

type Env = M.Map T.Text T.Text

data ProgOutput = PrintStdout | DontPrintStdout

mySystem :: LogLevel -> ProgOutput -> FilePath -> [String] -> Maybe Env -> Action ()
mySystem ll output prog args mEnv = do
  let cmd = unwords $ prog : args
  liftIO $ doLog ll cmd
  (secs, res) <-
    withTiming $
      traced (takeBaseName prog) $
        withDevNull $ \devNull -> do
          let myEnv = flip fmap mEnv $ \m -> map (bimap T.unpack T.unpack) $ M.toList m
              process =
                let p = (proc prog args) {env = myEnv}
                 in case output of
                      PrintStdout -> p
                      DontPrintStdout -> p {std_out = UseHandle devNull}
          (_, _, _, p) <- createProcess process
          waitForProcess p
  case res of
    ExitFailure i -> fail ("Command failed with exit code " ++ show i ++ ": " ++ cmd)
    ExitSuccess -> do
      let perfMsg = printf "%.3fs: %s" secs cmd
      if (secs >= 0.25) then note perfMsg else info perfMsg
      return ()

myReadFileBs :: FilePath -> Action BS.ByteString
myReadFileBs fp =
  do
    need [fp]
    debug ("Reading file " ++ fp)
    liftIO $ BS.readFile fp

myReadFile :: FilePath -> Action T.Text
myReadFile fp =
  do
    bs <- myReadFileBs fp
    case T.decodeUtf8' bs of
      Right t -> return t
      Left err -> fail ("File " ++ fp ++ " is not valid UTF-8: " ++ show err)

myReadFileIfExists :: FilePath -> Action (Maybe T.Text)
myReadFileIfExists fp = do
  myReadFileIfExistsGen fp myReadFile

myReadFileBsIfExists :: FilePath -> Action (Maybe BS.ByteString)
myReadFileBsIfExists fp = do
  myReadFileIfExistsGen fp myReadFileBs

myReadFileIfExistsGen :: FilePath -> (FilePath -> Action a) -> Action (Maybe a)
myReadFileIfExistsGen fp action = do
  ex <- Development.Shake.doesFileExist fp
  if ex
    then do
      t <- action fp
      return (Just t)
    else do
      dir <- liftIO getCurrentDirectory
      debug ("Not reading " ++ fp ++ " because it does not exist (cwd: " ++ dir ++ ")")
      return Nothing

myWriteFile :: FilePath -> T.Text -> Action ()
myWriteFile fp t =
  do
    debug ("Writing file " ++ fp)
    liftIO $ createDirectoryIfMissing True (takeDirectory fp)
    let bs = T.encodeUtf8 t
    liftIO $ BS.writeFile fp bs

showText :: Show a => a -> T.Text
showText = T.pack . show

newtype Hash = Hash {unHash :: T.Text}
  deriving (Eq, Ord, Show)

md5OfByteString :: BS.ByteString -> Hash
md5OfByteString bs =
  let h = MD5.hash bs
   in Hash $ T.decodeUtf8 $ Base16.encode h

md5OfText :: T.Text -> Hash
md5OfText t =
  md5OfByteString (T.encodeUtf8 t)

md5OfFile :: FilePath -> IO Hash
md5OfFile fp = do
  bs <- BS.readFile fp
  return $ md5OfByteString bs

needWithHash :: FilePath -> Action Hash
needWithHash fp = do
  need [fp]
  liftIO $ md5OfFile fp

isJpegFile :: FilePath -> Bool
isJpegFile fp = takeExtension fp `elem` [".jpg", ".jpeg"]

isPdfFile :: FilePath -> Bool
isPdfFile fp = takeExtension fp == ".pdf"

myListDirectory :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
myListDirectory dir pred = do
  fs <- listDirectory dir
  return (map (\x -> dir </> x) (filter pred fs))

-- Note: no cycle checking is performed
myListDirectoryRecursive :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
myListDirectoryRecursive dir pred = do
  fs <- listDirectory dir
  moreFiles <- forM fs $ \x -> do
    let p = dir </> x
    isDir <- System.Directory.doesDirectoryExist p
    if isDir && not (x `elem` [".", ".."])
      then myListDirectoryRecursive p pred
      else pure []
  let thisFiles = (map (\x -> dir </> x) (filter pred fs))
  pure (thisFiles ++ concat moreFiles)

isPathPrefix :: FilePath -> FilePath -> Bool
isPathPrefix prefix full =
  normalise prefix `L.isPrefixOf` normalise full

markdownImage :: FilePath -> (Maybe T.Text, Maybe T.Text) -> Bool -> T.Text
markdownImage path (width, height) center =
  let attrs =
        case catMaybes
          [ fmap (\w -> "width=" <> w) width,
            fmap (\w -> "height=" <> w) height,
            if center then Just ".Center" else Nothing
          ] of
          [] -> ""
          l -> "{" <> T.intercalate " " l <> "}"
   in "![](" <> T.pack path <> ")" <> attrs

removeAllFilesInDirectory :: FilePath -> IO ()
removeAllFilesInDirectory path = do
  cs <- map (path </>) <$> (listDirectory path `catch` (\(_ :: IOError) -> pure []))
  forM_ cs $ \p -> do
    isDir <- System.Directory.doesDirectoryExist p
    if isDir then removeDirectoryRecursive p else removeFile p

-- | Extracts a substring from the given text. Start is inclusive, end exclusive.
subText :: Int -> T.Text -> Int -> T.Text
subText start t end =
  let (_, rest) = T.splitAt start t
   in fst $ T.splitAt (end - start) rest

-- | Replaces in the given text. Start is inclusive, end exclusive.
replaceText :: Int -> T.Text -> Int -> T.Text -> T.Text
replaceText start t end toInsert =
  let before = subText 0 t start
      after = subText (max start end) t (T.length t)
   in before <> toInsert <> after

-- | Inserts into the given text. Start is inclusive, end exclusive.
insertText :: Int -> T.Text -> T.Text -> T.Text
insertText idx t toInsert = replaceText idx t idx toInsert

-- | Deletes a substring from the given text. Start is inclusive, end exclusive.
deleteText :: Int -> T.Text -> Int -> T.Text
deleteText start t end = replaceText start t end ""
