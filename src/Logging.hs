module Logging (

    LogLevel(..), doLog, trace, traceIO, debug, debugIO, note, noteIO
  , info, infoIO, warn, warnIO
  , setLogLevel, getLogLevel, isVerbose, isDebug, isNote, isTrace
  , logError, logErrorIO

) where

import Development.Shake
import Control.Concurrent.MVar
import Control.Monad
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
    do ll <- readMVar logLevelMVar
       return $ ll <= NOTE

isVerbose :: IO Bool
isVerbose =
    do ll <- readMVar logLevelMVar
       return $ ll <= INFO

isDebug :: IO Bool
isDebug =
    do ll <- readMVar logLevelMVar
       return $ ll <= DEBUG

isTrace :: IO Bool
isTrace =
    do ll <- readMVar logLevelMVar
       return $ ll <= TRACE

doLog :: LogLevel -> String -> IO ()
doLog ll msg =
    withMVar logLevelMVar $ \curLevel ->
        when (ll >= curLevel) (hPutStrLn stderr msg >> hFlush stderr)

trace :: String -> Action ()
trace = liftIO . traceIO

traceIO :: String -> IO ()
traceIO msg = doLog TRACE ("[TRACE] " ++ msg)

debug :: String -> Action ()
debug = liftIO . debugIO

debugIO :: String -> IO ()
debugIO msg = doLog DEBUG ("[DEBUG] " ++ msg)

note :: String -> Action ()
note = liftIO . noteIO

noteIO :: String -> IO ()
noteIO msg = doLog NOTE msg

info :: String -> Action ()
info = liftIO . infoIO

infoIO :: String -> IO ()
infoIO msg = doLog INFO ("[INFO] " ++ msg)

warn :: String -> Action ()
warn = liftIO . warnIO

warnIO :: String -> IO ()
warnIO msg = doLog WARN msg

logError :: String -> Action ()
logError = liftIO . logErrorIO

logErrorIO :: String -> IO ()
logErrorIO msg = doLog ERROR msg
