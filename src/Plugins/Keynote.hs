{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Plugins.Keynote
  ( keynotePlugin,
    keynoteFileToBuildPath,
    buildPathToKeynoteFile,
  )
where

import Control.Monad.Trans.Except
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Development.Shake
import Development.Shake.FilePath
import Logging
import Paths_text_slides
import System.Directory
import Text.Printf
import Types
import Utils

keynoteExportScript :: FilePath -> FilePath
keynoteExportScript dataDir = dataDir </> "data/keynote-export.py"

data KeynoteArgs = KeynoteArgs
  { ka_file :: FilePath,
    ka_slide :: Int
  }

parseArgs :: PluginCall -> Fail KeynoteArgs
parseArgs call = do
  ka_file <- T.unpack <$> getRequiredStringValue loc "file" argMap
  ka_slide <- getRequiredIntValue loc "slide" argMap
  return KeynoteArgs {..}
  where
    loc = pc_location call
    argMap = pc_args call

keynotePluginName :: PluginName
keynotePluginName = PluginName "keynote"

keynotePlugin :: PluginConfig Action
keynotePlugin =
  PluginConfig
    { p_name = keynotePluginName,
      p_kind = PluginWithoutBody,
      p_rules = pluginRules,
      p_expand = runPlugin,
      p_forAllCalls = processAllCalls
    }

-- | Convert a whole file path into a string that can be used as a path component, i.e.
-- the resulting string does slashes. This function is reversible (see singlePathComponentToPath)
-- and different pathes map to different strings.

-- relative path without ..    : just prefix the relative path with build/plugins/keynote/rel
-- relative path containing .. : strip leading slash and prefix with build/plugins/keynote/abs use
keynoteFileToBuildPath :: GenericBuildConfig m -> FilePath -> IO FilePath
keynoteFileToBuildPath cfg (normalise -> keynoteFile) =
  normaliseEx <$> do
    relOrAbs <- makeRelativeToCurrentDirectory keynoteFile
    let topDir = pluginDir cfg keynotePluginName
    case splitDirectories relOrAbs of
      components@("/" : _) -> return $ handleAbs topDir (joinPath components)
      components -> do
        let containsUpwardRefs = ".." `elem` components
        if containsUpwardRefs
          then do
            abs <- makeAbsolute relOrAbs
            return $ handleAbs topDir abs
          else return (topDir </> "rel" </> relOrAbs)
  where
    handleAbs topDir abs = topDir </> "abs" ++ abs

buildPathToKeynoteFile :: GenericBuildConfig m -> FilePath -> Fail FilePath
buildPathToKeynoteFile cfg (normalise -> fp) = do
  let topDir = pluginDir cfg keynotePluginName
      absDir = topDir </> "abs/"
      relDir = topDir </> "rel/"
  if
      | absDir `L.isPrefixOf` fp -> Right $ "/" ++ (drop (length absDir) fp)
      | relDir `L.isPrefixOf` fp -> Right (drop (length relDir) fp)
      | otherwise -> Left ("not a build path: " <> T.pack fp)

-- build/plugins/keynote/<my_presentation>/slides/slides.001.jpeg ~~>
-- build/plugins/keynote/<my_presentation>/presentation.keyhash
slideImagePathToPresentationHashPath :: FilePath -> Maybe FilePath
slideImagePathToPresentationHashPath (normalise -> fp) =
  let comps = splitDirectories fp
   in case reverse comps of
        (file : "slides" : rest)
          | isJpegFile file -> Just $ joinPath (reverse rest) </> "presentation.keyhash"
          | otherwise -> Nothing
        _ -> Nothing

-- build/plugins/keynote/<my_presentation>/presentation.keyhash ~~>
-- path to <my_presentation>
keynoteFileFromHashFile :: BuildConfig -> FilePath -> Fail FilePath
keynoteFileFromHashFile cfg fp =
  buildPathToKeynoteFile cfg (takeDirectory fp)

-- hashFile is some like build/plugins/keynote/<my_presentation>/presentation.keyhash
runKeynoteExport :: BuildConfig -> FilePath -> Action ()
runKeynoteExport cfg hashFile = do
  keyFile <- failInM $ keynoteFileFromHashFile cfg hashFile
  dataDir <- liftIO getDataDir
  let script = keynoteExportScript dataDir
      outDir = takeDirectory hashFile
  liftIO $ removeDirectoryRecursive (outDir </> "slides")
  note ("Exporting images from " ++ keyFile)
  hash <- needWithHash keyFile
  let exportArgs = [script, "-k", keyFile, "-o", outDir]
  mySystem INFO (bc_python cfg) exportArgs
  myWriteFile hashFile (unHash hash)

pluginRules :: BuildConfig -> BuildArgs -> Rules ()
pluginRules cfg _args = do
  isKeynoteJpeg ?> \jpg -> need [fromJust (slideImagePathToPresentationHashPath jpg)]
  "//presentation.keyhash" %> runKeynoteExport cfg
  where
    isKeynoteJpeg fp = isJust (slideImagePathToPresentationHashPath fp)

runPlugin :: BuildConfig -> BuildArgs -> PluginCall -> ExceptT T.Text Action T.Text
runPlugin cfg _buildArgs call = do
  args <- exceptInM $ parseArgs call
  dir <- liftIO $ keynoteFileToBuildPath cfg (ka_file args)
  -- all output files are place directly in the build directory
  let relDir = makeRelative (bc_buildDir cfg) dir
      imgFile = relDir </> "slides" </> printf "slides.%03d.jpeg" (ka_slide args)
  return $ "![](" <> T.pack imgFile <> ")"

processAllCalls ::
  BuildConfig -> BuildArgs -> [PluginCall] -> ExceptT T.Text Action ()
processAllCalls _cfg _buildArgs _calls = return ()
