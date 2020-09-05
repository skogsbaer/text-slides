{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Cmdline
  ( CmdlineOpts (..),
    parseCmdlineArgs,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T
import Development.Shake
import Options.Applicative
import Types

readOutputModes :: T.Text -> Maybe (Set.Set OutputMode)
readOutputModes t =
  let texts =
        filter (\s -> s /= "") $
          map T.strip $
            T.splitOn "," t
   in do
        l <- mapM readOutputMode texts
        return (Set.fromList l)

showOutputModes :: Set.Set OutputMode -> T.Text
showOutputModes modes =
  T.intercalate "," (map showOutputMode $ Set.toList modes)

data CmdlineOpts = CmdlineOpts
  { co_inputFile :: !FilePath,
    co_outputs :: !(Set.Set OutputMode),
    co_debug :: !Bool,
    co_verbose :: !Bool,
    co_quiet :: !Bool,
    co_shakeVerbosity :: !Verbosity,
    co_shakeProfile :: !Bool,
    co_jobs :: !Int
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
  co_verbose <- switch $ long "verbose" <> help "Display more output"
  co_debug <- switch $ long "debug" <> help "Display debug messages"
  co_quiet <- switch $ long "quiet" <> help "Be quiet"
  co_shakeProfile <- switch $ long "shake-profile" <> help "Produce a shake profile file"
  co_shakeVerbosity <-
    option auto $
      long "shake-verbosity"
        <> metavar "VERBOSITY"
        <> help
          ("Set verbosity level for shake, one of " ++ show [minBound .. maxBound :: Verbosity])
        <> value Warn
  co_jobs <-
    option auto $
      long "jobs"
        <> metavar "N"
        <> help "Set number of jobs"
        <> value 2
  -- inputFile should come last
  co_inputFile <-
    strArgument $
      metavar "INPUT_FILE.md"
        <> help "Input file in extended markdown format"
  pure CmdlineOpts {..}

cmdlineOptsParserInfo :: ParserInfo CmdlineOpts
cmdlineOptsParserInfo =
  info
    (cmdlineOptsParser <**> helper)
    ( fullDesc
        <> progDesc "Build a slide show from an extended markdown format."
    )

parseCmdlineArgs :: IO CmdlineOpts
parseCmdlineArgs = execParser cmdlineOptsParserInfo
