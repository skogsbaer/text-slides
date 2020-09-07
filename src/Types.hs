module Types where

import Control.Monad.Trans.Except
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import Development.Shake
import Safe
import System.FilePath
import Utils

type Fail a = Either T.Text a

failInM :: MonadFail m => Fail a -> m a
failInM (Right x) = return x
failInM (Left err) = fail (T.unpack err)

exceptInM :: Monad m => Fail a -> ExceptT T.Text m a
exceptInM (Right x) = return x
exceptInM (Left err) = throwE err

data OutputMode
  = OutputHtml
  | OutputPdf
  deriving (Eq, Ord, Show, Enum, Bounded)

outputModeStringMapping :: [(OutputMode, T.Text)]
outputModeStringMapping =
  [ (OutputHtml, "html"),
    (OutputPdf, "pdf")
  ]

readOutputMode :: T.Text -> Maybe OutputMode
readOutputMode s = L.lookup s (map (\(x, y) -> (y, x)) outputModeStringMapping)

showOutputMode :: OutputMode -> T.Text
showOutputMode m =
  fromJustNote ("unknown output mode: " ++ show m) $ L.lookup m outputModeStringMapping

outputModeToExtension :: OutputMode -> T.Text
outputModeToExtension mode = "." <> showOutputMode mode

data BuildArgs = BuildArgs
  {ba_inputFile :: FilePath}

type PluginMap m = M.Map PluginName (PluginConfig m)

data GenericBuildConfig m = BuildConfig
  { bc_buildDir :: FilePath,
    bc_pandoc :: FilePath,
    bc_python :: FilePath, -- python 3
    bc_plugins :: PluginMap m
  }

pluginDir :: GenericBuildConfig m -> PluginName -> FilePath
pluginDir cfg plugin = bc_buildDir cfg </> "plugins" </> T.unpack (unPluginName plugin)

type BuildConfig = GenericBuildConfig Action

newtype PluginName = PluginName {unPluginName :: T.Text}
  deriving (Eq, Ord, Show)

data PluginKind
  = PluginWithoutBody
  | PluginWithBody
  deriving (Eq, Show)

data ArgValue
  = ArgString T.Text
  | ArgInt Int
  | ArgBool Bool
  deriving (Eq, Show)

asString :: ArgValue -> Maybe T.Text
asString (ArgString t) = Just t
asString _ = Nothing

asInt :: ArgValue -> Maybe Int
asInt (ArgInt i) = Just i
asInt _ = Nothing

asBool :: ArgValue -> Maybe Bool
asBool (ArgBool b) = Just b
asBool _ = Nothing

castArgValue :: Location -> T.Text -> ArgValue -> T.Text -> (ArgValue -> Maybe a) -> Fail a
castArgValue l k v ty fun =
  case fun v of
    Just x -> Right x
    Nothing -> Left $ unLocation l <> ": Parameter " <> showText k <> " should have type " <> ty

type ArgMap = M.Map T.Text ArgValue

getRequiredValue :: Location -> T.Text -> ArgMap -> T.Text -> (ArgValue -> Maybe a) -> Fail a
getRequiredValue l k m ty fun =
  case M.lookup k m of
    Nothing -> Left $ unLocation l <> ": Missing required parameter " <> showText k
    Just v -> castArgValue l k v ty fun

getOptionalValue :: Location -> T.Text -> ArgMap -> T.Text -> (ArgValue -> Maybe a) -> Fail (Maybe a)
getOptionalValue l k m ty fun =
  case M.lookup k m of
    Nothing -> Right Nothing
    Just v -> fmap Just $ castArgValue l k v ty fun

getRequiredStringValue :: Location -> T.Text -> ArgMap -> Fail T.Text
getRequiredStringValue l k m =
  getRequiredValue l k m "String" asString

getOptionalStringValue :: Location -> T.Text -> ArgMap -> Fail (Maybe T.Text)
getOptionalStringValue l k m =
  getOptionalValue l k m "String" asString

getRequiredIntValue :: Location -> T.Text -> ArgMap -> Fail Int
getRequiredIntValue l k m =
  getRequiredValue l k m "Int" asInt

getOptionalIntValue :: Location -> T.Text -> ArgMap -> Fail (Maybe Int)
getOptionalIntValue l k m =
  getOptionalValue l k m "Int" asInt

getRequiredBoolValue :: Location -> T.Text -> ArgMap -> Fail Bool
getRequiredBoolValue l k m =
  getRequiredValue l k m "Bool" asBool

getOptionalBoolValue :: Location -> T.Text -> ArgMap -> Fail (Maybe Bool)
getOptionalBoolValue l k m =
  getOptionalValue l k m "Bool" asBool

newtype Location = Location {unLocation :: T.Text}
  deriving (Eq, Show)

unknownLocation :: Location
unknownLocation = Location "?:?"

data PluginCall = PluginCall
  { pc_pluginName :: PluginName,
    pc_location :: Location,
    pc_args :: ArgMap,
    pc_body :: T.Text
  }
  deriving (Eq, Show)

data PluginConfig action = PluginConfig
  { p_name :: PluginName,
    p_kind :: PluginKind,
    p_rules :: GenericBuildConfig action -> BuildArgs -> Rules (),
    p_expand ::
      GenericBuildConfig action ->
      BuildArgs ->
      PluginCall ->
      ExceptT T.Text action T.Text,
    p_forAllCalls ::
      GenericBuildConfig action ->
      BuildArgs ->
      [PluginCall] ->
      ExceptT T.Text Action ()
  }
