{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoreRules
  ( coreRules,
    transformMarkdown,
    mainOutputFile,
    mdRawOutputFile,
  )
where

import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as Hm
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Development.Shake
import LatexRules
import Logging
import Parser
import RuleUtils
import System.FilePath
import Types
import Utils
import Vars
import Plugins.AllPlugins

getDependenciesFromPandocJson :: FilePath -> IO [FilePath]
getDependenciesFromPandocJson inFile = do
  jsonValue <-
    J.eitherDecodeFileStrict' inFile >>= \case
      Right x -> return x
      Left s -> fail ("Cannot parse JSON file " ++ inFile ++ ": " ++ s)
  let deps = extractDeps jsonValue S.empty
  return (S.toList deps)
  where
    -- We parse the JSON explicitly instead of relying on pandoc-types.
    -- Reason: pandoc-types must match the version of the pandoc binary installed
    -- on your system
    extractDeps :: J.Value -> S.Set FilePath -> S.Set FilePath
    extractDeps val acc =
      case val of
        J.Object hm ->
          case (Hm.lookup "t" hm, Hm.lookup "c" hm) of
            (Just "Image", Just (J.Array args)) ->
              case getDepFromImageArgs args of
                Just t -> S.insert t acc
                Nothing -> acc
            _ -> foldr extractDeps acc (Hm.elems hm)
        J.Array values -> foldr extractDeps acc (V.toList values)
        _ -> acc
    getDepFromImageArgs :: V.Vector J.Value -> Maybe FilePath
    getDepFromImageArgs args
      | V.length args >= 3 =
        let arg = args V.! 2
         in case arg of
              J.Array subArgs
                | V.length subArgs >= 1 ->
                  case subArgs V.! 0 of
                    J.String t -> Just (T.unpack t)
                    _ -> Nothing
              _ -> Nothing
      | otherwise = Nothing

data PandocMode = PandocModeHtml | PandocModeLatex

runPandoc :: BuildConfig -> BuildArgs -> PandocMode -> FilePath -> FilePath -> Action ()
runPandoc cfg _args mode inFile {- .json -} outFile {- .html or .tex -} = do
  need [inFile]
  deps <- fmap (map (\f -> buildDir </> f)) $ liftIO $ getDependenciesFromPandocJson inFile
  writeDeps outFile deps
  syntaxDefs <- do
    let files = mapMaybe elc_syntaxFile (unExternalLangConfigs $ bc_externalLangConfigs cfg)
    forM files $ \f -> do
      need [f]
      return ("--syntax-definition=" ++ f)
  hightlightTheme <-
    forM (bc_syntaxTheme cfg) $ \x -> do
      case x of
        SyntaxThemeName name -> return (T.unpack name)
        SyntaxThemeFile f -> do
          need [f]
          return f
  needIfSet (bc_luaFilter cfg)
  let commonPandocArgs =
        syntaxDefs
          ++ [ "--from=json",
               "--slide-level=2",
               "--highlight-style=" ++ fromMaybe "pygments" hightlightTheme,
               "--output=" ++ outFile
             ]
          ++ optIfSet "--lua-filter=" (bc_luaFilter cfg)
      latexArgs = do
        need (bc_beamerHeader cfg)
        return $
          "--to=beamer" :
          (flip map (bc_beamerHeader cfg) $ \h -> "--include-in-header=" ++ h)
      htmlArgs = do
        needIfSet (bc_htmlHeader cfg)
        return $
          [ "--to=slidy",
            "--mathjax",
            "--standalone"
          ]
            ++ optIfSet "--include-in-header=" (bc_htmlHeader cfg)
  modePandocArgs <-
    case mode of
      PandocModeHtml -> htmlArgs
      PandocModeLatex -> latexArgs
  let pandocArgs = commonPandocArgs ++ modePandocArgs ++ [inFile]
  note ("Generating " ++ outFile)
  mySystem INFO DontPrintStdout (bc_pandoc cfg) pandocArgs Nothing
  where
    needIfSet (Just x) = need [x]
    needIfSet Nothing = return ()
    optIfSet prefix (Just x) = [prefix ++ x]
    optIfSet _ Nothing = []

data AnyPluginWithState action
  = forall state.
    AnyPluginWithState (PluginConfig state action) state

transformMarkdown ::
  forall m.
  MonadFail m =>
  (T.Text -> m ()) ->
  BuildConfig ->
  BuildArgs ->
  PluginMap m ->
  FilePath ->
  T.Text ->
  m (T.Text, [PluginCall])
transformMarkdown warnFun cfg buildArgs pm inFile md = do
  tokens <- failInM $ parseMarkdown inFile pluginKindMap md
  (revLines, _) <- foldM tokenToLine ([], M.empty) tokens
  let calls =
        flip mapMaybe tokens $ \case
          Line _ -> Nothing
          Plugin call -> Just call
  return (T.unlines $ reverse revLines, calls)
  where
    tokenToLine ::
      ([T.Text], M.Map PluginName (AnyPluginWithState m)) ->
      Token ->
      m ([T.Text], M.Map PluginName (AnyPluginWithState m))
    tokenToLine (acc, stateMap) tok =
      case tok of
        Line t -> return (t : acc, stateMap)
        Plugin call -> do
          old <- getPluginWithState (pc_pluginName call) stateMap
          case old of
            AnyPluginWithState plugin pluginState -> do
              pluginRes <- runExceptT $ p_expand plugin cfg buildArgs pluginState call
              case pluginRes of
                Right (t, newState) ->
                  let newPluginWithState = AnyPluginWithState plugin newState
                   in return (t : acc, M.insert (pc_pluginName call) newPluginWithState stateMap)
                Left err -> do
                  warnFun (unLocation (pc_location call) <> ": plugin call failed: " <> err)
                  -- insert the old state into the map, it might have been just initialized
                  return (acc, M.insert (pc_pluginName call) old stateMap)
    getPluginWithState ::
      PluginName -> M.Map PluginName (AnyPluginWithState m) -> m (AnyPluginWithState m)
    getPluginWithState pluginName stateMap =
      case M.lookup pluginName stateMap of
        Just x -> return x
        Nothing ->
          case M.lookup pluginName pm of
            Nothing ->
              error $
                "BUG: Plugin " ++ show pluginName
                  ++ " present after parsing but plugin not in plugin map"
            Just (AnyPluginConfig plugin) -> do
              state <- p_init plugin
              return (AnyPluginWithState plugin state)
    pluginKindMap =
      flip M.map pm $ \(AnyPluginConfig plugin) -> p_kind plugin

generateRawMarkdown :: BuildConfig -> BuildArgs -> FilePath -> FilePath -> Action ()
generateRawMarkdown cfg args inFile outFile = do
  md' <- myReadFile inFile
  let varsFile = ba_inputFile args -<.> "yaml"
  varsFileExists <- doesFileExist varsFile
  md <- case varsFileExists of
    False -> do
      info ("Variables file " ++ varsFile ++ " does not exist")
      pure md'
    True -> do
      note ("Reading variables from " ++ varsFile)
      vars <- readVarsFile varsFile
      pure (expandVars vars md')
  let pm = pluginMap cfg
  (rawMd, calls) <- transformMarkdown (warn . T.unpack) cfg args pm inFile md
  let callMap = foldr (\call m -> M.insertWith (++) (pc_pluginName call) [call] m) M.empty calls
  forM_ (M.toList callMap) $ \(pluginName, calls) -> do
    res <-
      case M.lookup pluginName pm of
        Nothing ->
          error $
            "BUG: Plugin with name " ++ show pluginName ++ " missing in plugin map"
        Just (AnyPluginConfig plugin) ->
          runExceptT $ p_forAllCalls plugin cfg args calls
    case res of
      Left err ->
        warn
          ( "Processing all calls of plugin " ++ T.unpack (unPluginName pluginName)
              ++ " failed: "
              ++ T.unpack err
          )
      Right () -> return ()
  myWriteFile outFile rawMd

generateJson :: BuildConfig -> FilePath -> FilePath -> Action ()
generateJson cfg inFile {- .mdraw -} outFile {- .json -} = do
  need [inFile]
  note $ "Generating " ++ outFile
  mySystem INFO DontPrintStdout (bc_pandoc cfg) ["--from=markdown", "--output=" ++ outFile, inFile] Nothing

coreRules :: BuildArgs -> Rules ()
coreRules args = do
  -- We do not use pandoc for generating pdf because we want to get better performance.
  -- Pandoc just blindly reruns pdflatex 2 times, even if it's not necessary. We further
  -- speed up compilation by caching the preamble with mylatexformat. To make this work,
  -- the latex source code must contain \endofdump at the start of a line.
  [outFile ".html"] &%> \[html] -> do
    cfg <- getBuildConfig
    void $ runPandoc cfg args PandocModeHtml json html
  [outFile ".tex", outFile ".deps"] &%> \[tex, _] -> do
    cfg <- getBuildConfig
    void $ runPandoc cfg args PandocModeLatex json tex
  latexRules args
  raw %> \out -> do
    cfg <- getBuildConfig
    generateRawMarkdown cfg args (ba_inputFile args) out
  json %> \out -> do
    cfg <- getBuildConfig
    generateJson cfg raw out
  allPluginRules args
  isStaticOutFile ?> publishStaticFile
  where
    outFile ext = mainOutputFile args ext
    raw = outFile mdRawExt
    json = outFile ".json"
    isStaticOutFile f =
      (isImage f || isStaticPdf f)
        && buildDir `isPathPrefix` f
        && not (pluginDir' `isPathPrefix` f)
    publishStaticFile out =
      copyFileChanged (outputFileToInputFile args out) out
    isImage f = takeExtension f `elem` [".jpg", ".jpeg", ".png"]
    isStaticPdf f =
      takeExtension f == ".pdf"
        && takeBaseName (ba_inputFile args) /= takeBaseName f
