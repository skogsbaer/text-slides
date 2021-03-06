{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-

Each call of the mermaid plugin expands into a markdown reference to the
image build/plugins/mermaid/<HASH>.png, where <HASH> is the MD5 hash of the
plugin call. During the expansion step, we generate the files
build/plugins/mermaid/<HASH>.(json|mdd). There is then a build rule generating
<HASH>.png from <HASH>.json and <HASH>.mdd. The JSON file contains the commandline
arguments for mermaid, the .mdd file the code for the diagram.

-}
module Plugins.Mermaid
  ( mermaidPlugin,
  )
where

import Control.Monad
import Control.Monad.Trans.Except
import CoreRules
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Development.Shake.FilePath
import GHC.Generics
import Logging
import System.Directory
import Types
import Utils

data MermaidArgs = MermaidArgs
  { ma_width :: Maybe Int,
    ma_height :: Maybe Int
  }

parseArgs :: PluginCall -> Fail MermaidArgs
parseArgs call = do
  ma_width <- getOptionalIntValue loc "width" argMap
  ma_height <- getOptionalIntValue loc "height" argMap
  checkForSpuriousArgs loc argMap ["width", "height"]
  return MermaidArgs {..}
  where
    loc = pc_location call
    argMap = pc_args call

mermaidPluginName :: PluginName
mermaidPluginName = PluginName "mermaid"

mermaidPlugin :: AnyPluginConfig Action
mermaidPlugin =
  AnyPluginConfig $
    PluginConfig
      { p_name = mermaidPluginName,
        p_kind = PluginWithBody,
        p_rules = pluginRules,
        p_init = return (),
        p_expand = runPlugin,
        p_forAllCalls = processAllCalls
      }

runMermaid :: BuildConfig -> BuildArgs -> FilePath -> Action ()
runMermaid cfg _buildArgs pngFile = do
  let jsonFile = replaceExtension pngFile ".json"
      mddFile = replaceExtension pngFile ".mdd"
  need [jsonFile, mddFile]
  json <- liftIO $ BSL.readFile jsonFile
  case J.decode' json of
    Nothing ->
      warn
        ( "Cannot read " ++ show jsonFile ++ ", cannot run mermaid. "
            ++ "Try to remove the build directory"
        )
    Just mermaidCall -> do
      note
        ( "Running mermaid for diagram at " ++ T.unpack (mc_where mermaidCall) ++ " to produce "
            ++ pngFile
        )
      let mermaidArgs =
            map T.unpack (mc_args mermaidCall)
              ++ ["--input", mddFile, "--output", pngFile, "--scale", "8"]
      mySystem INFO (bc_mermaid cfg) mermaidArgs Nothing

pluginRules :: BuildConfig -> BuildArgs -> Rules ()
pluginRules cfg args = do
  (pluginDir cfg mermaidPluginName) ++ "/*.png" %> runMermaid cfg args
  (pluginDir cfg mermaidPluginName) ++ "/*.json" %> \_ -> need [mdRawOutputFile cfg args]
  (pluginDir cfg mermaidPluginName) ++ "/*.mdd" %> \_ -> need [mdRawOutputFile cfg args]

data MermaidCall = MermaidCall
  { mc_where :: T.Text,
    mc_args :: [T.Text]
  }
  deriving (Generic, Show)

instance J.FromJSON MermaidCall

instance J.ToJSON MermaidCall

mermaidCallAndHash :: Monad m => PluginCall -> ExceptT T.Text m (MermaidCall, Hash)
mermaidCallAndHash call = do
  args <- exceptInM $ parseArgs call
  let mermaidCall =
        MermaidCall
          { mc_args = toArg "--width" (ma_width args) ++ toArg "--height" (ma_height args),
            mc_where = unLocation (pc_location call)
          }
      hash = md5OfText (showText mermaidCall <> pc_body call)
  return (mermaidCall, hash)
  where
    toArg _ Nothing = []
    toArg opt (Just i) = [opt, showText i]

runPlugin :: BuildConfig -> BuildArgs -> () -> PluginCall -> ExceptT T.Text Action (T.Text, ())
runPlugin cfg _buildArgs () call = do
  (mermaidCall, hash) <- mermaidCallAndHash call
  let outDir = pluginDir cfg mermaidPluginName
      outFile ext =
        pluginDir cfg mermaidPluginName </> T.unpack (unHash hash) <.> ext
      diagFile = outFile ".png"
      -- all output files are place directly in the build directory
      relDiagFile = makeRelative (bc_buildDir cfg) diagFile
  liftIO $ createDirectoryIfMissing True outDir
  liftIO $ J.encodeFile (outFile ".json") mermaidCall
  liftIO $ T.writeFile (outFile ".mdd") (pc_body call)
  let res = "![](" <> T.pack relDiagFile <> ")"
  return (res, ())

processAllCalls ::
  BuildConfig -> BuildArgs -> [PluginCall] -> ExceptT T.Text Action ()
processAllCalls cfg _buildArgs calls = do
  -- remove unreference files
  allHashes <- forM calls $ \call -> mermaidCallAndHash call >>= (return . snd)
  let setOfHashes = S.fromList (map unHash allHashes)
  files <-
    liftIO $
      myListDirectory (pluginDir cfg mermaidPluginName) $ \f ->
        takeExtension f `elem` [".png", ".json", ".mdd"]
  forM_ files $ \file -> do
    let hash = T.pack $ takeBaseName file
    unless (hash `S.member` setOfHashes) (liftIO $ removeFile file)
