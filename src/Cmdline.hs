{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Cmdline
  ( CmdlineOpts (..),
    parseCmdlineArgs,
  )
where

import qualified Data.Set as S
import qualified Data.Text as T
import Development.Shake
import Options.Applicative
import Data.Traversable
import Types

readOutputModes :: T.Text -> Maybe (S.Set OutputMode)
readOutputModes t =
  let texts =
        filter (/= "") $
          map T.strip $
            T.splitOn "," t
   in do
        l <- mapM readOutputMode texts
        return (S.fromList l)

showOutputModes :: S.Set OutputMode -> T.Text
showOutputModes modes =
  T.intercalate "," (map showOutputMode $ S.toList modes)

data CmdlineOpts = CmdlineOpts
  { co_inputFile :: !(Maybe FilePath),
    co_beamerHeader :: Maybe FilePath,
    co_htmlHeader :: Maybe FilePath,
    co_luaFilter :: Maybe FilePath,
    co_mermaidConfig :: Maybe FilePath,
    co_syntaxTheme :: Maybe FilePath,
    co_outputs :: !(S.Set OutputMode),
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
  outputs <-
    option (maybeReader (readOutputModes . T.pack)) $
      long "output-mode"
        <> metavar "MODES"
        <> help
          ( T.unpack
              ( "Comma-separated list of output modes (" <> showOutputModes allOutputModes
                  <> "), default: pdf"
              )
          )
        <> value S.empty
  moreOutputs <-
    for (S.toList allOutputModes) $ \mode -> do
      enable <-
        switch $
          long (T.unpack $ showOutputMode mode)
            <> help (T.unpack $ "Enable output mode " <> showOutputMode mode)
      pure (enable, mode)
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
  co_beamerHeader <-
    optional $
      option str $
        long "beamer-header"
          <> metavar "FILE"
          <> help
            ( "File to insert into the header of a beamer presentation. Can also be set by "
                ++ "placing the file beamer-header.tex next to the input file or inside "
                ++ "$HOME/.text-slides. All files found are place in the header of the "
                ++ "presentation."
            )
  co_htmlHeader <-
    optional $
      option str $
        long "html-header"
          <> metavar "FILE"
          <> help
            ( "File to insert into the header of a html presentation. Can also be set by "
                ++ "placing the file html-header.html next to the input file or inside "
                ++ "$HOME/.text-slides."
            )
  co_luaFilter <-
    optional $
      option str $
        long "lua-filter"
          <> metavar "FILE"
          <> help
            ( "Lua filter for pandoc. Can also be set by "
                ++ "placing the file pandoc-filter.lua next to the input file or inside "
                ++ "$HOME/.text-slides."
            )
  co_mermaidConfig <-
    optional $
      option str $
        long "mermaid-config"
          <> metavar "FILE"
          <> help
            ( "JSON file for configuring mermaid. Can also be set by "
                ++ "placing the file mermaid-config.json next to the input file or inside "
                ++ "$HOME/.text-slides."
            )
  co_syntaxTheme <-
    optional $
      option str $
        long "syntax-theme"
          <> metavar "NAME_OF_THEME|FILE"
          <> help
            ( "Name of a syntax highlighting theme or file with a defintion of a syntax "
                ++ "highlighting theme. A file defining such a theme can also be set by placing"
                ++ "the file syntax-highlighting.theme next to the input file or inside "
                ++ "$HOME/.text-slides."
            )
  -- inputFile should come last
  co_inputFile <-
    optional $
      strArgument $
        metavar "INPUT_FILE.md"
          <> help
            ( "Input file in extended markdown format. Can be omitted if the current directory"
                ++ " contains exactly one file with extension .md"
            )
  pure $
    let co_outputs =
          let s =
                outputs
                  `S.union` (S.fromList (map snd (filter fst moreOutputs)))
           in if S.null s then S.singleton OutputPdf else s
     in CmdlineOpts {..}

cmdlineOptsParserInfo :: ParserInfo CmdlineOpts
cmdlineOptsParserInfo =
  info
    (cmdlineOptsParser <**> helper)
    ( fullDesc
        <> progDesc "Build a slide show from an extended markdown format."
    )

parseCmdlineArgs :: IO CmdlineOpts
parseCmdlineArgs = execParser cmdlineOptsParserInfo
