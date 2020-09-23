module Utils where

import Control.Exception
import Control.Monad.IO.Class
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as L
import qualified Data.Map as M
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

mySystem :: LogLevel -> FilePath -> [String] -> Maybe Env -> Action ()
mySystem ll prog args mEnv = do
  let cmd = unwords $ prog : args
  liftIO $ doLog ll cmd
  (secs, res) <-
    withTiming $
      traced (takeBaseName prog) $
        withDevNull $ \devNull -> do
          let myEnv = flip fmap mEnv $ \m -> map (\(x, y) -> (T.unpack x, T.unpack y)) $ M.toList m
              process = (proc prog args) {std_out = UseHandle devNull, env = myEnv}
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
    bs <- liftIO $ BS.readFile fp
    debug ("Finished reading file " ++ fp)
    return bs

myReadFile :: FilePath -> Action T.Text
myReadFile fp =
  do
    bs <- myReadFileBs fp
    case T.decodeUtf8' bs of
      Right t -> return t
      Left err -> fail ("File " ++ fp ++ " is not valid UTF-8: " ++ show err)

myWriteFile :: FilePath -> T.Text -> Action ()
myWriteFile fp t =
  do
    debug ("Writing file " ++ fp)
    liftIO $ createDirectoryIfMissing True (takeDirectory fp)
    let bs = T.encodeUtf8 t
    liftIO $ BS.writeFile fp bs
    debug ("Finished writing file " ++ fp)

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
  h <- liftIO $ md5OfFile fp
  return h

isJpegFile :: FilePath -> Bool
isJpegFile fp = takeExtension fp `elem` [".jpg", ".jpeg"]

myListDirectory :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
myListDirectory dir pred = do
  fs <- listDirectory dir
  return (map (\x -> dir </> x) (filter pred fs))

isPathPrefix :: FilePath -> FilePath -> Bool
isPathPrefix prefix full =
  normalise prefix `L.isPrefixOf` normalise full
