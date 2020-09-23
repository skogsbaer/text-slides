{-# LANGUAGE ExistentialQuantification #-}
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
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Development.Shake
import Logging
import Parser
import System.FilePath
import Types
import Utils

getDependenciesFromPandocJson :: FilePath -> IO [FilePath]
getDependenciesFromPandocJson inFile = do
  jsonValue <-
    J.eitherDecodeFileStrict' inFile >>= \res ->
      case res of
        Right x -> return x
        Left s -> fail ("Cannot parse JSON file " ++ inFile ++ ": " ++ s)
  let deps = extractDeps jsonValue S.empty
  return (S.toList deps)
  where
    -- We parse the JSON explicitly instead of relying on pandoc-types.
    -- Reason: pandoc-types must must the version of the pandoc binary installed
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

runPandoc :: GenericBuildConfig m -> BuildArgs -> OutputMode -> FilePath -> FilePath -> Action ()
runPandoc cfg _args mode inFile {- .json -} outFile {- .html or .pdf -} = do
  need [inFile]
  deps <- liftIO $ getDependenciesFromPandocJson inFile
  -- The deps are relative to the build dir
  need (map (\f -> bc_buildDir cfg </> f) deps)
  syntaxDefs <-
    flip mapM (V.toList $ bc_syntaxDefFiles cfg) $ \f -> do
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
               "--highlight-style=" ++ fromMaybe "haddock" hightlightTheme,
               "--output=" ++ outFile,
               "--resource-path=" ++ bc_buildDir cfg
             ]
          ++ optIfSet "--lua-filter=" (bc_luaFilter cfg)
      latexArgs = do
        needIfSet (bc_beamerHeader cfg)
        return $
          ["--to=beamer"]
            ++ optIfSet "--include-in-header=" (bc_beamerHeader cfg)
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
      OutputHtml -> htmlArgs
      OutputPdf -> latexArgs
      OutputLatex -> latexArgs
  let pandocArgs = commonPandocArgs ++ modePandocArgs ++ [inFile]
  note ("Generating " ++ outFile)
  mySystem INFO (bc_pandoc cfg) pandocArgs
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
  GenericBuildConfig m ->
  BuildArgs ->
  FilePath ->
  T.Text ->
  m (T.Text, [PluginCall])
transformMarkdown warnFun cfg buildArgs inFile md = do
  tokens <- failInM $ parseMarkdown inFile pluginKindMap md
  (revLines, _) <- foldM tokenToLine ([], M.empty) tokens
  let calls =
        flip mapMaybe tokens $ \tok ->
          case tok of
            Line _ -> Nothing
            Plugin call -> Just call
  return $ (T.unlines $ reverse revLines, calls)
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
          case M.lookup pluginName (bc_plugins cfg) of
            Nothing ->
              error $
                "BUG: Plugin " ++ show pluginName
                  ++ " present after parsing but plugin not in plugin map"
            Just (AnyPluginConfig plugin) -> do
              state <- p_init plugin
              return (AnyPluginWithState plugin state)
    pluginKindMap =
      flip M.map (bc_plugins cfg) $ \pws ->
        case pws of
          AnyPluginConfig plugin -> p_kind plugin

generateRawMarkdown :: BuildConfig -> BuildArgs -> FilePath -> FilePath -> Action ()
generateRawMarkdown cfg args inFile outFile = do
  md <- myReadFile inFile
  (rawMd, calls) <- transformMarkdown (warn . T.unpack) cfg args inFile md
  let callMap = foldr (\call m -> M.insertWith (++) (pc_pluginName call) [call] m) M.empty calls
  forM_ (M.toList callMap) $ \(pluginName, calls) -> do
    res <-
      case M.lookup pluginName (bc_plugins cfg) of
        Nothing ->
          error $
            "BUG: Plugin with name " ++ show pluginName ++ " missing in plugin map"
        Just (AnyPluginConfig plugin) ->
          runExceptT $ p_forAllCalls plugin cfg args calls
    case res of
      Left err -> do
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
  mySystem INFO (bc_pandoc cfg) ["--from=markdown", "--output=" ++ outFile, inFile]

coreRules :: BuildConfig -> BuildArgs -> Rules ()
coreRules cfg args = do
  forM_ [minBound .. maxBound :: OutputMode] $ \mode ->
    outFile (T.unpack $ outputModeToExtension mode) %> runPandoc cfg args mode json
  raw %> generateRawMarkdown cfg args (ba_inputFile args)
  json %> generateJson cfg raw
  sequence_ $ map (\(_, AnyPluginConfig p) -> p_rules p cfg args) (M.toList (bc_plugins cfg))
  bc_buildDir cfg ++ "//*.png" %> publishStaticFile
  bc_buildDir cfg ++ "//*.jpg" %> publishStaticFile
  where
    outFile ext = mainOutputFile cfg args ext
    raw = outFile mdRawExt
    json = outFile ".json"
    publishStaticFile out = do
      copyFileChanged (outputFileToInputFile cfg out) out

mdRawExt :: String
mdRawExt = ".mdraw"

-- To keep things simple, the output file for the presentation is always placed at the
-- toplevel of the build dir. This means, that two input files must not have the same
-- basename.
mainOutputFile :: BuildConfig -> BuildArgs -> String -> FilePath
mainOutputFile cfg args ext =
  bc_buildDir cfg </> takeBaseName (ba_inputFile args) <.> ext

mdRawOutputFile :: BuildConfig -> BuildArgs -> FilePath
mdRawOutputFile cfg args = mainOutputFile cfg args mdRawExt

outputFileToInputFile :: BuildConfig -> FilePath -> FilePath
outputFileToInputFile cfg out =
  let prefix = bc_buildDir cfg ++ "/"
   in if not (prefix `L.isPrefixOf` out)
        then error ("outputFileToInputFile: not an output file: " ++ out)
        else bc_searchDir cfg </> drop (length prefix) out
