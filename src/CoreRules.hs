module CoreRules (
    coreRules
) where

import Logging
import Utils
import Types
import Parser

import Development.Shake
import System.FilePath
import qualified Data.Map.Strict as M
import qualified Data.Text as T

runPandoc :: BuildConfig -> OutputMode -> FilePath -> FilePath -> Action ()
runPandoc cfg mode inFile outFile = do
    need [inFile]
    let commonPandocArgs =
            [ "--from=markdown"
            , "slide-level=2"
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
    mySystem INFO (bc_pandoc cfg) pandocArgs

transformMarkdown :: MonadFail m => BuildConfig -> FilePath -> T.Text -> m T.Text
transformMarkdown _cfg inFile md = do
    tokens <- failInM $ parseMarkdown inFile pluginMap md
    return $ T.unlines (tokensToLines tokens)
  where
    tokensToLines = concatMap tokenToLines
    tokenToLines tok =
        case tok of
            Line t -> [t]
            Plugin _ -> [] -- FIXME!
    pluginMap = M.empty -- FIXME

generateRawMarkdown :: BuildConfig -> FilePath -> FilePath -> Action ()
generateRawMarkdown cfg inFile outFile = do
    md <- myReadFile inFile
    rawMd <- transformMarkdown cfg inFile md
    myWriteFile outFile rawMd
coreRules :: BuildConfig -> BuildArgs -> Rules ()
coreRules cfg args = do
    outFile ".html" %> runPandoc cfg OutputHtml raw
    outFile ".pdf" %> runPandoc cfg OutputPdf raw
    raw %> generateRawMarkdown cfg (ba_inputFile args)
  where
    outFile ext = bc_buildDir cfg </> replaceExtension (ba_inputFile args) ext
    raw = outFile ".mdraw"
