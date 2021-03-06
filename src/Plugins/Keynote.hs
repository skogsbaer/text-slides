{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-

Each call keynote(file: "presi.key", slide: 42) expands into a markdown reference to the
image build/plugins/keynote/presi.key/slides/slide.042.jpeg. For each of such jpeg images,
there is a build rule depending on build/plugins/keynote/presi.key/presentation.hash. The
build rule for this hash file runs the export for all images and generates the hash file in
the end.

-}
module Plugins.Keynote
  ( keynotePlugin,
    keynoteFileToBuildPath,
    buildPathToKeynoteFile,
  )
where

import Control.Monad
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
    ka_slide :: Int,
    ka_width :: Maybe T.Text,
    ka_height :: Maybe T.Text,
    ka_center :: Bool
  }

parseArgs :: PluginCall -> Fail KeynoteArgs
parseArgs call = do
  ka_file <- T.unpack <$> getRequiredStringValue loc "file" argMap
  ka_slide <- getRequiredIntValue loc "slide" argMap
  ka_width <- getOptionalValue loc "width" argMap "Int or String" intOrStringAsString
  ka_height <- getOptionalValue loc "height" argMap "Int or String" intOrStringAsString
  ka_center <- fromMaybe False <$> getOptionalBoolValue loc "center" argMap
  checkForSpuriousArgs loc argMap ["file", "slide", "width", "height", "center"]
  return KeynoteArgs {..}
  where
    loc = pc_location call
    argMap = pc_args call
    intOrStringAsString v =
      case v of
        ArgString t -> Just t
        ArgInt i -> Just $ showText i
        ArgBool _ -> Nothing

keynotePluginName :: PluginName
keynotePluginName = PluginName "keynote"

keynotePlugin :: AnyPluginConfig Action
keynotePlugin =
  AnyPluginConfig $
    PluginConfig
      { p_name = keynotePluginName,
        p_kind = PluginWithoutBody,
        p_rules = pluginRules,
        p_init = return (),
        p_expand = runPlugin,
        p_forAllCalls = processAllCalls
      }

-- | Convert a keynote file into a path in the build directory where the results of extracting
-- the images of the presentation will be placed. The inverse of this function is
-- `buildPathToKeynoteFile`.
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

-- | Inverse of `keynoteFileToBuildPath`
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

-- hashFile is something like build/plugins/keynote/<my_presentation>/presentation.keyhash
runKeynoteExport :: BuildConfig -> FilePath -> Action ()
runKeynoteExport cfg hashFile = do
  keyFile <- failInM $ buildPathToKeynoteFile cfg (takeDirectory hashFile)
  dataDir <- liftIO getDataDir
  let script = keynoteExportScript dataDir
      outDir = takeDirectory hashFile
  liftIO $ removeDirectoryRecursive (outDir </> "slides")
  note ("Exporting images from " ++ keyFile)
  hash <- needWithHash keyFile
  let exportArgs = [script, "-k", keyFile, "-o", outDir]
  mySystem INFO (bc_python cfg) exportArgs Nothing
  jpegs <- liftIO $ myListDirectory (outDir </> "slides") isJpegFile
  forM_ jpegs $ \jpeg ->
    let (d, f) = splitFileName jpeg
     in mySystem
          INFO
          (bc_convert cfg)
          [jpeg, "-trim", d </> ("trimmed_" ++ fixImageFileName f)]
          Nothing
  myWriteFile hashFile (unHash hash)
  where
    fixImageFileName f =
      -- latex does not like images ending with .001.jpeg
      let slideNo = tail $ takeExtension (dropExtension f)
       in "slides_" ++ slideNo ++ ".jpg"

pluginRules :: BuildConfig -> BuildArgs -> Rules ()
pluginRules cfg _args = do
  isKeynoteJpeg ?> \jpg -> need [fromJust (slideImagePathToPresentationHashPath jpg)]
  "//presentation.keyhash" %> runKeynoteExport cfg
  where
    isKeynoteJpeg fp = isJust (slideImagePathToPresentationHashPath fp)

runPlugin :: BuildConfig -> BuildArgs -> () -> PluginCall -> ExceptT T.Text Action (T.Text, ())
runPlugin cfg _buildArgs () call = do
  args <- exceptInM $ parseArgs call
  dir <- liftIO $ keynoteFileToBuildPath cfg (ka_file args)
  -- all output files are place directly in the build directory
  let relDir = makeRelative (bc_buildDir cfg) dir
      imgFile = relDir </> "slides" </> printf "trimmed_slides_%03d.jpg" (ka_slide args)
      dimensions =
        case catMaybes
          [ fmap (\w -> "width=" <> w) (ka_width args),
            fmap (\w -> "height=" <> w) (ka_height args)
          ] of
          [] -> ""
          l -> "{" <> T.intercalate " " l <> "}"
      prefix =
        case ka_center args of
          True -> "![center]"
          False -> "![]"
      res = prefix <> "(" <> T.pack imgFile <> ")" <> dimensions
  return (res, ())

processAllCalls ::
  BuildConfig -> BuildArgs -> [PluginCall] -> ExceptT T.Text Action ()
processAllCalls _cfg _buildArgs _calls = return ()
