module Types where

import qualified Data.Text as T

type Fail a = Either T.Text a

failInM :: MonadFail m => Fail a -> m a
failInM (Right x) = return x
failInM (Left err) = fail (T.unpack err)

data OutputMode
    = OutputHtml
    | OutputPdf
    deriving (Eq, Ord, Show)

data BuildArgs
    = BuildArgs
    { ba_inputFile :: FilePath }

data BuildConfig
    = BuildConfig
    { bc_buildDir :: FilePath,
      bc_pandoc :: FilePath
    }
