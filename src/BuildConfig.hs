{-# LANGUAGE RecordWildCards #-}

module BuildConfig
  ( getBuildConfig,
    ExternalLangConfig (..),
    ExternalLangConfigs (..),
  )
where

import Cmdline
import Control.Monad
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as J
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import Development.Shake hiding (doesFileExist)
import Logging
import Plugins.Code
import Plugins.Keynote
import Plugins.Mermaid
import System.Directory
import System.FilePath
import Types

allPlugins :: [(T.Text, LangConfig)] -> [AnyPluginConfig Action]
allPlugins moreLanguages = [keynotePlugin, mermaidPlugin] ++ (codePlugins moreLanguages)

getHomeCfgDir :: IO FilePath
getHomeCfgDir = do
  home <- getHomeDirectory
  return $ home </> ".text-slides"

getBuildConfig :: CmdlineOpts -> IO BuildConfig
getBuildConfig opts = do
  current <- getCurrentDirectory
  let searchDir =
        case co_inputFile opts of
          Just f -> takeDirectory f
          Nothing -> current
      searchFile = searchFile' searchDir
  beamerHeader <- searchFile "beamer-header.tex" (co_beamerHeader opts) >>= canonicalize
  infoIO ("beamerHeader: " ++ show beamerHeader)
  htmlHeader <- searchFile "html-header.html" (co_htmlHeader opts) >>= canonicalize
  infoIO ("htmlHeader: " ++ show htmlHeader)
  luaFilter <- searchFile "pandoc-filter.lua" (co_luaFilter opts) >>= canonicalize
  infoIO ("luaFilter: " ++ show luaFilter)
  syntaxTheme <- do
    mf <- searchFile "syntax-highlighting.theme" (co_syntaxTheme opts)
    case mf of
      Nothing -> return Nothing
      Just f -> do
        b <- doesFileExist f
        if b
          then (Just . SyntaxThemeFile) <$> canonicalizePath f
          else return (Just $ SyntaxThemeName (T.pack f))
  infoIO ("syntaxHighlighting: " ++ show syntaxTheme)
  externalCfgs <- V.toList . unExternalLangConfigs <$> getExternLangConfigs
  infoIO ("External code plugins: " ++ show externalCfgs)
  return $
    BuildConfig
      { bc_buildDir = "build",
        bc_pandoc = "pandoc",
        bc_pdflatex = "pdflatex",
        bc_python = "python3",
        bc_convert = "convert",
        bc_mermaid = "mmdc",
        bc_beamerHeader = beamerHeader,
        bc_htmlHeader = htmlHeader,
        bc_luaFilter = luaFilter,
        bc_syntaxTheme = syntaxTheme,
        bc_plugins =
          M.fromList $
            map
              (\(AnyPluginConfig p) -> (p_name p, AnyPluginConfig p))
              (allPlugins (map languageFromExternal externalCfgs)),
        bc_syntaxDefFiles = V.fromList $ mapMaybe elc_syntaxFile externalCfgs,
        bc_verbose = co_verbose opts,
        bc_searchDir = searchDir
      }
  where
    canonicalize Nothing = return Nothing
    canonicalize (Just p) = do
      cp <- canonicalizePath p
      return (Just cp)
    searchFile' :: FilePath -> FilePath -> Maybe FilePath -> IO (Maybe FilePath)
    searchFile' _ _ (Just fromCmdLine) = return $ Just fromCmdLine
    searchFile' searchDir path Nothing = do
      homeCfgDir <- getHomeCfgDir
      let candidates = [searchDir </> path, homeCfgDir </> path]
      results <- forM candidates $ \cand -> do
        b <- doesFileExist cand
        return $ if b then Just cand else Nothing
      case catMaybes results of
        [] -> return Nothing
        (x : _) -> return $ Just x
    languageFromExternal cfg =
      ( elc_name cfg,
        LangConfig
          { lc_fileExt = elc_fileExt cfg,
            lc_commentStart = elc_commentStart cfg,
            lc_commentEnd = elc_commentEnd cfg
          }
      )

data ExternalLangConfig = ExternalLangConfig
  { elc_name :: T.Text,
    elc_fileExt :: String,
    elc_commentStart :: T.Text,
    elc_commentEnd :: Maybe T.Text,
    elc_syntaxFile :: Maybe FilePath
  }
  deriving (Show, Eq)

newtype ExternalLangConfigs = ExternalLangConfigs {unExternalLangConfigs :: V.Vector ExternalLangConfig}
  deriving (Show, Eq)

instance J.FromJSON ExternalLangConfig where
  parseJSON = J.withObject "ExternalLangConfig" $ \v -> do
    elc_name <- v .: "name"
    elc_syntaxFile <- v .:? "syntaxFile"
    elc_fileExt <- v .: "extension"
    elc_commentStart <- v .: "commentStart"
    elc_commentEnd <- v .:? "commentEnd"
    return ExternalLangConfig {..}

instance J.FromJSON ExternalLangConfigs where
  parseJSON = J.withObject "ExternalLangConfigs" $ \v -> do
    langs <- v .: "languages"
    return $ ExternalLangConfigs langs

getExternLangConfigs :: IO ExternalLangConfigs
getExternLangConfigs = do
  homeCfgDir <- getHomeCfgDir
  let f = homeCfgDir </> "code.json"
  b <- doesFileExist f
  if not b
    then return $ ExternalLangConfigs V.empty
    else loadExternalLangConfigs f
  where
    loadExternalLangConfigs f = do
      res <- J.eitherDecodeFileStrict f
      case res of
        Left err -> fail ("Error parsing contents of " ++ f ++ ": " ++ err)
        Right (ExternalLangConfigs vs) ->
          return $ ExternalLangConfigs $ V.map (makeRel (takeDirectory f)) vs
    makeRel dir cfg =
      cfg {elc_syntaxFile = fmap (\x -> dir </> x) (elc_syntaxFile cfg)}
