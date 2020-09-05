module Utils where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Logging
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
    let bs = T.encodeUtf8 t
    liftIO $ BS.writeFile fp bs
    debug ("Finished writing file " ++ fp)
