{-# OPTIONS_GHC -Wno-orphans #-}
module BuildConfig
  ( computeBuildConfig, computeBuildArgs,
    ExternalLangConfig (..),
    ExternalLangConfigs (..),
  )
where

import Cmdline
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Development.Shake
import Logging
import Plugins.Code
import System.Directory hiding (doesFileExist, doesDirectoryExist)
import qualified System.Directory as Dir
import System.Exit
import System.FilePath
import Types
import Utils

getHomeCfgDir :: IO FilePath
getHomeCfgDir = do
  home <- getHomeDirectory
  return $ home </> ".text-slides"

computeBuildArgs :: CmdlineOpts -> IO BuildArgs
computeBuildArgs opts = do
  inputFile <-
    case co_inputFile opts of
      Just f -> return f
      Nothing -> do
        files <- myListDirectory "." $ \f ->
          takeExtension f == ".md" && not ("." `L.isPrefixOf` (takeBaseName f))
        case files of
          [f] -> return f
          [] -> do
            putStrLn ("No input file given and no .md file exists in current directory.")
            exitWith (ExitFailure 1)
          files -> do
            putStrLn
              ( "No input file given and multiple .md file exist in current directory: "
                  ++ show files
              )
            exitWith (ExitFailure 1)
  exists <- Dir.doesFileExist inputFile
  unless exists $ do
    putStrLn $ "Input file " ++ inputFile ++ " does not exist, aborting!"
    exitWith (ExitFailure 1)
  let searchDir = takeDirectory inputFile
      args =
        BuildArgs
          { ba_inputFile = inputFile,
            ba_searchDir = searchDir,
            ba_verbose = co_verbose opts
          }
  pure args

computeBuildConfig :: CmdlineOpts -> BuildArgs -> Action BuildConfig
computeBuildConfig opts args = do
  beamerHeader <-
    searchFiles "beamer-header.tex" (co_beamerHeader opts) >>= mapM (liftIO . canonicalizePath)
  info ("beamerHeader: " ++ show beamerHeader)
  htmlHeader <- searchFile "html-header.html" (co_htmlHeader opts) >>= canonicalize
  info ("htmlHeader: " ++ show htmlHeader)
  luaFilter <- searchFile "pandoc-filter.lua" (co_luaFilter opts) >>= canonicalize
  info ("luaFilter: " ++ show luaFilter)
  mermaidConfig <- searchFile "mermaid-config.json" (co_mermaidConfig opts) >>= canonicalize
  info ("mermaidConfig: " ++ show luaFilter)
  syntaxTheme <- do
    mf <- searchFile "syntax-highlighting.theme" (co_syntaxTheme opts)
    case mf of
      Nothing -> return Nothing
      Just f -> do
        b <- doesFileExist f
        if b
          then (Just . SyntaxThemeFile) <$> liftIO (canonicalizePath f)
          else return (Just $ SyntaxThemeName (T.pack f))
  info ("syntaxHighlighting: " ++ show syntaxTheme)
  externalCfgs <- computeExternLangConfigs
  info ("External code plugins: " ++ show externalCfgs)
  let cfg =
        BuildConfig
          { bc_pandoc = "pandoc",
            bc_pdflatex = "pdflatex",
            bc_python = "python3",
            bc_convert = "convert",
            bc_mermaid = "mmdc",
            bc_pdfcrop = "pdfcrop",
            bc_beamerHeader = beamerHeader,
            bc_htmlHeader = htmlHeader,
            bc_luaFilter = luaFilter,
            bc_mermaidConfig = mermaidConfig,
            bc_syntaxTheme = syntaxTheme,
            bc_externalLangConfigs = externalCfgs
          }
  return cfg
  where
    canonicalize Nothing = return Nothing
    canonicalize (Just p) = do
      cp <- liftIO (canonicalizePath p)
      return (Just cp)
    searchFile = searchFile' (ba_searchDir args)
    searchFiles = searchFiles' (ba_searchDir args)

searchFile' :: FilePath -> FilePath -> Maybe FilePath -> Action (Maybe FilePath)
searchFile' searchDir path mCmdLine = do
  allFiles <- searchFiles' searchDir path mCmdLine
  case reverse allFiles of
    [] -> return Nothing
    (x:_) -> return (Just x)

searchFiles' :: FilePath -> FilePath -> Maybe FilePath -> Action [FilePath]
searchFiles' searchDir path mCmdLine = do
  homeCfgDir <- liftIO getHomeCfgDir
  let candidates = [homeCfgDir </> path, searchDir </> path]
  results <- forM candidates $ \cand -> do
    b <- doesFileExist cand
    return $ if b then Just cand else Nothing
  return $ catMaybes (results ++ [mCmdLine])

computeExternLangConfigs :: Action ExternalLangConfigs
computeExternLangConfigs = do
  homeCfgDir <- liftIO getHomeCfgDir
  let f = homeCfgDir </> "code.json"
  b <- doesFileExist f
  if not b
    then return $ ExternalLangConfigs []
    else loadExternalLangConfigs f
  where
    loadExternalLangConfigs f = do
      bs <- myReadFileBs f
      case J.eitherDecodeStrict bs of
        Left err -> fail ("Error parsing contents of " ++ f ++ ": " ++ err)
        Right (ExternalLangConfigs vs) ->
          return $ ExternalLangConfigs $ map (makeRel (takeDirectory f)) vs
    makeRel dir cfg =
      cfg {elc_syntaxFile = fmap (\x -> dir </> x) (elc_syntaxFile cfg)}
