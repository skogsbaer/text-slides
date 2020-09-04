module CoreRules (
    coreRules, transformMarkdown
) where

import Logging
import Utils
import Types
import Parser

import Control.Monad
import Development.Shake
import System.FilePath
import Control.Monad.Trans.Except
import Control.Monad.Extra
import qualified Data.Map.Strict as M
import qualified Data.Text as T

runPandoc :: GenericBuildConfig m -> OutputMode -> FilePath -> FilePath -> Action ()
runPandoc cfg mode inFile outFile = do
    need [inFile]
    let commonPandocArgs =
            [ "--from=markdown"
            , "--slide-level=2"
            , "--highlight-style=pygments"
            , "--output=" ++ outFile
            ]
        modePandocArgs =
            case mode of
                OutputHtml ->
                    [ "--to=slidy"
                    , "--mathjax"
                    , "--standalone"
                    ]
                OutputPdf -> error "PDF not yet implemented"
        pandocArgs = commonPandocArgs ++ modePandocArgs ++ [inFile]
    mySystem NOTE (bc_pandoc cfg) pandocArgs

transformMarkdown ::
    MonadFail m => (T.Text -> m ()) -> [PluginConfig m] -> FilePath -> T.Text -> m T.Text
transformMarkdown warnFun plugins inFile md = do
    tokens <- failInM $ parseMarkdown inFile pluginKindMap md
    lines <- mapMaybeM tokenToLine tokens
    return $ T.unlines lines
  where
    tokenToLine tok =
        case tok of
            Line t -> return (Just t)
            Plugin call ->
                case M.lookup (pc_pluginName call) pluginMap of
                    Nothing ->
                        error $
                            "BUG: Plugin call " ++ show call ++
                            " present after parsing but plugin not in plugin map"
                    Just plugin -> do
                        pluginRes <- runExceptT $ p_expand plugin call
                        case pluginRes of
                            Right t -> return (Just t)
                            Left err -> do
                                warnFun (pc_location call <> ": plugin call failed: " <> err)
                                return Nothing
    pluginMap =
        M.fromList $
        flip map plugins $ \plugin -> (p_name plugin, plugin)
    pluginKindMap =
        M.fromList $
        flip map plugins $ \plugin -> (p_name plugin, p_kind plugin)

generateRawMarkdown :: BuildConfig -> FilePath -> FilePath -> Action ()
generateRawMarkdown cfg inFile outFile = do
    md <- myReadFile inFile
    rawMd <- transformMarkdown (warn . T.unpack) (bc_plugins cfg) inFile md
    -- FIXME: extract dependencies
    myWriteFile outFile rawMd

coreRules :: BuildConfig -> BuildArgs -> Rules ()
coreRules cfg args = do
    forM_ [minBound..maxBound] $ \mode ->
        outFile (T.unpack $ outputModeToExtension mode) %> runPandoc cfg mode raw
    raw %> generateRawMarkdown cfg (ba_inputFile args)
    sequence_ $ map p_rules (bc_plugins cfg)
  where
    outFile ext = bc_buildDir cfg </> replaceExtension (ba_inputFile args) ext
    raw = outFile ".mdraw"
