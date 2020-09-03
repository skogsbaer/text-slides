module Types where

import Safe
import qualified Data.Text as T
import qualified Data.List as L

type Fail a = Either T.Text a

failInM :: MonadFail m => Fail a -> m a
failInM (Right x) = return x
failInM (Left err) = fail (T.unpack err)

data OutputMode
    = OutputHtml
    | OutputPdf
    deriving (Eq, Ord, Show, Enum, Bounded)

outputModeStringMapping :: [(OutputMode, T.Text)]
outputModeStringMapping =
    [ (OutputHtml, "html")
    , (OutputPdf, "pdf")
    ]

readOutputMode :: T.Text -> Maybe OutputMode
readOutputMode s = L.lookup s (map (\(x, y) -> (y, x)) outputModeStringMapping)

showOutputMode :: OutputMode -> T.Text
showOutputMode m =
    fromJustNote ("unknown output mode: " ++ show m) $ L.lookup m outputModeStringMapping

outputModeToExtension :: OutputMode -> T.Text
outputModeToExtension mode = "." <> showOutputMode mode

data BuildArgs
    = BuildArgs
    { ba_inputFile :: FilePath }

data BuildConfig
    = BuildConfig
    { bc_buildDir :: FilePath,
      bc_pandoc :: FilePath
    }
