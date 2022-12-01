module Temp
    ( withSysTempFile, withSysTempDir
    , withTempFile, withTempDir, defaultTemporaryDirectory
    , ignoringIOErrors
    , DeleteStrategy(..), deleteIfNoException, deleteAfterAction, deleteManually
    )
where


----------------------------------------
-- LOCAL
----------------------------------------

----------------------------------------
-- SITE-PACKAGES
----------------------------------------
import qualified System.IO.Temp as Temp

----------------------------------------
-- STDLIB
----------------------------------------
import Control.Exception
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import System.IO
import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T
import qualified System.Directory as Dir
import qualified System.IO as IO

data DeleteStrategy a
    = DeleteOnResult T.Text {- reason -} (SomeException -> Bool) (a -> Bool)
      -- ^ Use the result of the action to determine whether it should be deleted or not.
      -- See 'deleteIfNoException', 'deleteAfterAction', and 'deleteManually' for the most important
      -- values.

deleteIfNoException :: DeleteStrategy a
deleteIfNoException = DeleteOnResult "deleteIfNoException" (const False) (const True)

-- | Delete after running the action, independant from the result.
deleteAfterAction :: DeleteStrategy a
deleteAfterAction = DeleteOnResult "deleteAfterAction" (const True) (const True)

deleteManually :: DeleteStrategy a
deleteManually = DeleteOnResult "deleteManually" (const False) (const False)

defaultTemporaryDirectory :: MonadIO m => m FilePath
defaultTemporaryDirectory = liftIO Dir.getTemporaryDirectory

withSysTempFile ::
    (MonadMask m, MonadIO m)
    => DeleteStrategy b
    -> String
    -> ((FilePath, IO.Handle) -> m b)
    -> m b
withSysTempFile strat template action =
    do tmpDir <- liftIO Dir.getTemporaryDirectory
       withTempFile strat tmpDir template action

withSysTempDir ::
    (MonadMask m, MonadIO m)
    => DeleteStrategy b
    -> String
    -> (FilePath -> m b)
    -> m b
withSysTempDir strat template action =
    do tmpDir <- liftIO Dir.getTemporaryDirectory
       withTempDir strat tmpDir template action

withTempFile ::
    (MonadMask m, MonadIO m)
    => DeleteStrategy b
    -> FilePath
    -> String
    -> ((FilePath, IO.Handle) -> m b)
    -> m b
withTempFile = withTempSomething create remove describe
    where
      create tmpDir template = Temp.openBinaryTempFile tmpDir template
      remove (name, hdl) = ignoringIOErrors (IO.hClose hdl >> Dir.removeFile name)
      describe (name, _) = "tmpfile " ++ name

-- | Creates a new temporary directory.  Note: the name of the temporary directory
-- will probably be the same if you call this function twice with the same arguments
-- and the directory does not exist.
withTempDir ::
    (MonadMask m, MonadIO m)
    => DeleteStrategy b
    -> FilePath
    -> String
    -> (FilePath -> m b)
    -> m b
withTempDir = withTempSomething create remove describe
    where
      create tmpDir template =
          do newDir <- Temp.createTempDirectory tmpDir template
             return newDir
      remove name = ignoringIOErrors (Dir.removeDirectoryRecursive name)
      describe name = "tmpdir " ++ name

logWarn :: String -> IO ()
logWarn msg = hPutStrLn stderr ("WARN: " ++ msg)

withTempSomething ::
    (MonadMask m, MonadIO m)
    => (FilePath -> String -> IO a)  -- ^ create
    -> (a -> IO ())                 -- ^ remove
    -> (a -> String)                -- ^ describe thing created
    -> DeleteStrategy b             -- ^ delete strategy
    -> FilePath                     -- ^ temporary directory
    -> String                       -- ^ name template
    -> (a -> m b)                   -- ^ action
    -> m b
withTempSomething create remove describe strat tmpDir template action =
    bracketWithResult
      (liftIO $ create tmpDir template)
      cleanupAfter
      action
    where
      cleanupAfter x res =
          case strat of
            DeleteOnResult name onExc onRegular ->
                case res of
                  Left exc | onExc exc -> liftIO $ remove x
                  Right y | onRegular y -> liftIO $ remove x
                  Left exc ->
                      do let msg =
                                 "Not removing temporary " ++ describe x ++ " because of " ++
                                 show exc ++ ": " ++ T.unpack name
                         liftIO $ logWarn msg
                  _ ->
                    do liftIO $
                         logWarn ("Not removing temporary " ++ describe x ++ ": " ++ T.unpack name)
                       return ()

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe =
    ioe `Control.Exception.catch` (\(_ :: IOError) -> return ())

bracketWithResult ::
    (MonadMask m)
    => m a                                  -- ^ computation to run first (\"acquire resource\")
    -> (a
        -> Either SomeException c
        -> m b)                             -- ^ computation to run last (\"release resource\")
    -> (a -> m c)                           -- ^ computation to run in-between
    -> m c                                  -- ^ returns the value from the in-between computation
bracketWithResult before after thing =
  Catch.mask $ \restore ->
      do a <- before
         r <- Catch.catch (restore (thing a)) $
                \(exc :: SomeException) ->
                    do _ <- after a (Left exc)
                       Catch.throwM exc
         _ <- after a (Right r)
         return r
