module CoreRules
  ( coreRules,
    transformMarkdown,
    mainOutputFile,
  )
where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Except
import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as Hm
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

runPandoc :: GenericBuildConfig m -> OutputMode -> FilePath -> FilePath -> Action ()
runPandoc cfg mode inFile {- .json -} outFile {- .html or .pdf -} = do
  need [inFile]
  deps <- liftIO $ getDependenciesFromPandocJson inFile
  -- The deps are relative to the build dir
  need (map (\x -> bc_buildDir cfg </> x) deps)
  let commonPandocArgs =
        [ "--from=json",
          "--slide-level=2",
          "--highlight-style=pygments",
          "--output=" ++ outFile
        ]
      modePandocArgs =
        case mode of
          OutputHtml ->
            [ "--to=slidy",
              "--mathjax",
              "--standalone"
            ]
          OutputPdf -> error "PDF not yet implemented"
      pandocArgs = commonPandocArgs ++ modePandocArgs ++ [inFile]
  note ("Generating " ++ outFile)
  mySystem INFO (bc_pandoc cfg) pandocArgs

getKnownPlugin :: PluginName -> PluginMap m -> PluginConfig m
getKnownPlugin pluginName pluginMap =
  case M.lookup pluginName pluginMap of
    Nothing ->
      error $
        "BUG: Plugin with name " ++ show pluginName ++ " missing in plugin map"
    Just plugin -> plugin

transformMarkdown ::
  MonadFail m =>
  (T.Text -> m ()) ->
  GenericBuildConfig m ->
  BuildArgs ->
  FilePath ->
  T.Text ->
  m (T.Text, [PluginCall])
transformMarkdown warnFun cfg buildArgs inFile md = do
  tokens <- failInM $ parseMarkdown inFile pluginKindMap md
  lines <- mapMaybeM tokenToLine tokens
  let calls =
        flip mapMaybe tokens $ \tok ->
          case tok of
            Line _ -> Nothing
            Plugin call -> Just call
  return $ (T.unlines lines, calls)
  where
    tokenToLine tok =
      case tok of
        Line t -> return (Just t)
        Plugin call ->
          case M.lookup (pc_pluginName call) (bc_plugins cfg) of
            Nothing ->
              error $
                "BUG: Plugin call " ++ show call
                  ++ " present after parsing but plugin not in plugin map"
            Just plugin -> do
              pluginRes <- runExceptT $ p_expand plugin cfg buildArgs call
              case pluginRes of
                Right t -> return (Just t)
                Left err -> do
                  warnFun (unLocation (pc_location call) <> ": plugin call failed: " <> err)
                  return Nothing
    pluginKindMap =
      M.map p_kind (bc_plugins cfg)

generateRawMarkdown :: BuildConfig -> BuildArgs -> FilePath -> FilePath -> Action ()
generateRawMarkdown cfg args inFile outFile = do
  md <- myReadFile inFile
  (rawMd, calls) <- transformMarkdown (warn . T.unpack) cfg args inFile md
  let callMap = foldr (\call m -> M.insertWith (++) (pc_pluginName call) [call] m) M.empty calls
  forM_ (M.toList callMap) $ \(pluginName, calls) -> do
    res <- runExceptT $ p_forAllCalls (getKnownPlugin pluginName (bc_plugins cfg)) cfg args calls
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
    outFile (T.unpack $ outputModeToExtension mode) %> runPandoc cfg mode json
  raw %> generateRawMarkdown cfg args (ba_inputFile args)
  json %> generateJson cfg raw
  sequence_ $ map (\(_, p) -> p_rules p cfg args) (M.toList (bc_plugins cfg))
  where
    outFile ext = mainOutputFile cfg args ext
    raw = outFile ".mdraw"
    json = outFile ".json"

-- To keep things simple, the output file for the presentation is always placed at the
-- toplevel of the build dir. This means, that two input files must not have the same
-- basename.
mainOutputFile :: BuildConfig -> BuildArgs -> String -> FilePath
mainOutputFile cfg args ext =
  bc_buildDir cfg </> takeBaseName (ba_inputFile args) <.> ext
