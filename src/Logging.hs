module Logging
  ( LogLevel (..),
    doLog,
    trace,
    traceIO,
    traceM,
    debug,
    debugIO,
    debugM,
    note,
    noteIO,
    info,
    infoIO,
    warn,
    warnIO,
    setLogLevel,
    getLogLevel,
    isVerbose,
    isDebug,
    isNote,
    isTrace,
    logError,
    logErrorIO,
  )
where

import Ansi
import Control.Concurrent.MVar
import Control.Monad
import Development.Shake
import System.IO
import System.IO.Unsafe (unsafePerformIO)

data LogLevel = TRACE | DEBUG | INFO | NOTE | WARN | ERROR
  deriving (Show, Eq, Ord)

logLevelMVar :: MVar LogLevel
logLevelMVar = unsafePerformIO (newMVar NOTE)
{-# NOINLINE logLevelMVar #-}

setLogLevel :: LogLevel -> IO ()
setLogLevel ll =
  modifyMVar_ logLevelMVar $ \_ -> return ll

getLogLevel :: IO LogLevel
getLogLevel =
  readMVar logLevelMVar

isNote :: IO Bool
isNote =
  do
    ll <- readMVar logLevelMVar
    return $ ll <= NOTE

isVerbose :: IO Bool
isVerbose =
  do
    ll <- readMVar logLevelMVar
    return $ ll <= INFO

isDebug :: IO Bool
isDebug =
  do
    ll <- readMVar logLevelMVar
    return $ ll <= DEBUG

isTrace :: IO Bool
isTrace =
  do
    ll <- readMVar logLevelMVar
    return $ ll <= TRACE

doLog :: LogLevel -> String -> IO ()
doLog ll msg =
  withMVar logLevelMVar $ \curLevel ->
    when (ll >= curLevel) (hPutStrLn stderr formatted >> hFlush stderr)
  where
    formatted =
      let x = levelPrefix ll ++ msg
       in case levelColor ll of
            Just c -> colorize c x
            Nothing -> x

levelColor :: LogLevel -> Maybe Ansi
levelColor ll =
  case ll of
    TRACE -> Just _DARK_GRAY_
    DEBUG -> Just _DARK_GRAY_
    INFO -> Nothing
    NOTE -> Nothing
    WARN -> Just _PURPLE_
    ERROR -> Just _LIGHT_RED_

levelPrefix :: LogLevel -> String
levelPrefix ll =
  case ll of
    TRACE -> "[TRACE] "
    DEBUG -> "[DEBUG] "
    INFO -> "[INFO] "
    NOTE -> ""
    WARN -> "WARN: "
    ERROR -> "ERROR: "

trace :: String -> Action ()
trace = liftIO . traceIO

traceIO :: String -> IO ()
traceIO = doLog TRACE

debug :: String -> Action ()
debug = liftIO . debugIO

debugIO :: String -> IO ()
debugIO = doLog DEBUG

note :: String -> Action ()
note = liftIO . noteIO

noteIO :: String -> IO ()
noteIO = doLog NOTE

info :: String -> Action ()
info = liftIO . infoIO

infoIO :: String -> IO ()
infoIO = doLog INFO

warn :: String -> Action ()
warn = liftIO . warnIO

warnIO :: String -> IO ()
warnIO = doLog WARN

logError :: String -> Action ()
logError = liftIO . logErrorIO

logErrorIO :: String -> IO ()
logErrorIO = doLog ERROR

debugM :: Monad m => String -> m ()
debugM msg =
  unsafePerformIO (debugIO msg) `seq` pure ()

traceM :: Monad m => String -> m ()
traceM msg =
  unsafePerformIO (debugIO msg) `seq` pure ()
