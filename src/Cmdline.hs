{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Cmdline (
    parseCmdlineArgs
) where

import Options.Applicative
import Safe
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Types

outputModeStringMapping :: [(OutputMode, T.Text)]
outputModeStringMapping =
    [ (OutputHtml, "html")
    , (OutputPdf, "pdf")
    ]

readOutputMode :: T.Text -> Maybe OutputMode
readOutputMode s = L.lookup s (map (\(x, y) -> (y, x)) outputModeStringMapping)

readOutputModes :: T.Text -> Maybe (Set.Set OutputMode)
readOutputModes t =
    let texts =
            filter (\s -> s /= "") $
            map T.strip $
            T.splitOn "," t
    in do
        l <- mapM readOutputMode texts
        return (Set.fromList l)

showOutputMode :: OutputMode -> T.Text
showOutputMode m =
    fromJustNote ("unknown output mode: " ++ show m) $ L.lookup m outputModeStringMapping

showOutputModes :: Set.Set OutputMode -> T.Text
showOutputModes modes =
    T.intercalate "," (map showOutputMode $ Set.toList modes)

data CmdlineOpts
    = CmdlineOpts
    { co_inputFile :: !FilePath
    , co_outputs :: !(Set.Set OutputMode)
    }
    deriving (Eq, Show)

cmdlineOptsParser :: Parser CmdlineOpts
cmdlineOptsParser = do
    co_outputs <-
        option (maybeReader (readOutputModes . T.pack)) $
            long "output-mode"
            <> metavar "MODES"
            <> help "Comma-separated list of output modes (html, pdf)"
            <> value (Set.singleton OutputHtml)
            <> showDefaultWith (T.unpack . showOutputModes)
    co_inputFile <-
        strArgument $
            metavar "INPUT_FILE.md" <>
            help "Input file in extended markdown format"
    pure CmdlineOpts{..}

cmdlineOptsParserInfo :: ParserInfo CmdlineOpts
cmdlineOptsParserInfo =
    info (cmdlineOptsParser <**> helper)
        (fullDesc
         <> progDesc "Build a slide show from an extended markdown format.")

parseCmdlineArgs :: IO CmdlineOpts
parseCmdlineArgs = execParser cmdlineOptsParserInfo
