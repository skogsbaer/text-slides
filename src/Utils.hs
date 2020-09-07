module Utils where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Logging
import System.Directory
import System.Exit
import System.FilePath
import System.Process

mySystem :: LogLevel -> FilePath -> [String] -> Action ()
mySystem ll prog args = do
  let cmd = unwords $ prog : args
  liftIO $ doLog ll cmd
  res <- traced (takeBaseName prog) $ rawSystem prog args
  case res of
    ExitFailure i -> fail ("Command failed with exit code " ++ show i ++ ": " ++ cmd)
    ExitSuccess -> return ()

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

md5OfFile :: FilePath -> IO Hash
md5OfFile fp = do
  bs <- BS.readFile fp
  let h = MD5.hash bs
  return (Hash $ T.decodeUtf8 $ Base16.encode h)

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
