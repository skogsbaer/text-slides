{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import Control.Monad.Trans.Except
import Data.Aeson as J
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Foldable (forM_)
import Development.Shake
import Development.Shake.Classes
import Safe
import System.FilePath
import Utils
import GHC.Generics (Generic)

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
  | OutputLatex
  deriving (Eq, Ord, Show, Enum, Bounded)

data InputMode
  = InputModeSlides
  | InputModeArticle
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData, Enum, Bounded)

outputModeStringMapping :: [(OutputMode, T.Text)]
outputModeStringMapping =
  [ (OutputHtml, "html"),
    (OutputPdf, "pdf"),
    (OutputLatex, "latex")
  ]

readOutputMode :: T.Text -> Maybe OutputMode
readOutputMode s = L.lookup s (map (\(x, y) -> (y, x)) outputModeStringMapping)

showOutputMode :: OutputMode -> T.Text
showOutputMode m =
  fromJustNote ("unknown output mode: " ++ show m) $
    L.lookup m outputModeStringMapping

outputModeToExtension :: OutputMode -> T.Text
outputModeToExtension m =
  fromJustNote ("unknown output mode: " ++ show m) $
    L.lookup m outputModeStringMapping

allOutputModes :: S.Set OutputMode
allOutputModes = S.fromList [minBound .. maxBound]

-- BuildArgs must not contain any file on which we need to put a dependency with need.
data BuildArgs = BuildArgs
  { ba_inputFile :: FilePath,
    ba_inputMode :: InputMode,
    ba_verbose :: Bool,
    ba_searchDir :: FilePath
  }
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

data SyntaxTheme
  = SyntaxThemeName T.Text
  | SyntaxThemeFile FilePath
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

data StaticBuildConfig
  = StaticBuildConfig
  { sbc_buildDir :: FilePath }
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

staticBuildConfig :: StaticBuildConfig
staticBuildConfig =
  StaticBuildConfig
  { sbc_buildDir = "build" }

data ExternalLangConfig = ExternalLangConfig
  { elc_name :: T.Text,
    elc_fileExt :: String,
    elc_commentStart :: T.Text,
    elc_commentEnd :: Maybe T.Text,
    elc_syntaxFile :: Maybe FilePath
  }
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

newtype ExternalLangConfigs = ExternalLangConfigs {unExternalLangConfigs :: [ExternalLangConfig]}
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

instance J.FromJSON ExternalLangConfig where
  parseJSON = J.withObject "ExternalLangConfig" $ \v -> do
    elc_name <- v .: "name"
    elc_syntaxFile <- v .:? "syntaxFile"
    elc_fileExt <- v .: "extension"
    elc_commentStart <- v .: "commentStart"
    elc_commentEnd <- v .:? "commentEnd"
    return ExternalLangConfig {..}

instance J.FromJSON ExternalLangConfigs where
  parseJSON = J.withObject "ExternalLangConfigs" $ \v -> do
    langs <- v .: "languages"
    return $ ExternalLangConfigs langs

data BuildConfig = BuildConfig
  { bc_pandoc :: FilePath,
    bc_pdflatex :: FilePath,
    bc_python :: FilePath, -- python 3
    bc_convert :: FilePath, -- imagemagick
    bc_mermaid :: FilePath,
    bc_pdfcrop :: FilePath,
    bc_beamerHeader :: [FilePath],
    bc_articleHeader :: [FilePath],
    bc_htmlHeader :: Maybe FilePath,
    bc_luaFilter :: Maybe FilePath,
    bc_mermaidConfig :: Maybe FilePath,
    bc_syntaxTheme :: Maybe SyntaxTheme,
    bc_externalLangConfigs :: ExternalLangConfigs
  }
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

buildDir :: FilePath
buildDir = sbc_buildDir staticBuildConfig

pluginDir' :: FilePath
pluginDir' = buildDir </> "plugins"

pluginDir :: PluginName -> FilePath
pluginDir plugin = pluginDir' </> T.unpack (unPluginName plugin)

newtype BuildConfigKey
  = BuildConfigKey ()
  deriving (Show,Typeable,Eq,Generic,Hashable,Binary,NFData)

type instance RuleResult BuildConfigKey = BuildConfig

getBuildConfig :: Action BuildConfig
getBuildConfig = askOracle (BuildConfigKey ())

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
    Just v -> Just <$> castArgValue l k v ty fun

getRequiredStringValue :: Location -> T.Text -> ArgMap -> Fail T.Text
getRequiredStringValue l k m =
  getRequiredValue l k m "String" asString

getOptionalStringValue :: Location -> T.Text -> ArgMap -> Fail (Maybe T.Text)
getOptionalStringValue l k m =
  getOptionalValue l k m "String" asString

checkEnum :: Location -> T.Text -> [T.Text] -> T.Text -> Fail ()
checkEnum loc k allowed v =
  if v `elem` allowed
    then return ()
    else
      Left $
        unLocation loc <> ": invalid value for key " <> k <> ", allowed are only "
          <> T.intercalate ", " allowed

getRequiredEnumValue :: Location -> T.Text -> [T.Text] -> ArgMap -> Fail T.Text
getRequiredEnumValue l k values m = do
  v <- getRequiredValue l k m "String" asString
  checkEnum l k values v
  return v

getOptionalEnumValue :: Location -> T.Text -> [T.Text] -> ArgMap -> Fail (Maybe T.Text)
getOptionalEnumValue l k values m = do
  mv <- getOptionalValue l k m "String" asString
  forM_ mv $ \v -> checkEnum l k values v
  return mv

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

checkForSpuriousArgs :: Location -> ArgMap -> [T.Text] -> Fail ()
checkForSpuriousArgs loc m ks =
  let presentKeys = M.keysSet m
      supportedKeys = S.fromList ks
      unsupportedKeys = presentKeys `S.difference` supportedKeys
   in if S.null unsupportedKeys
        then Right ()
        else
          let l = S.toList unsupportedKeys
              keyStr = if length l == 1 then "key" else "keys"
           in Left $
                unLocation loc <> ": Unkown argument " <> keyStr <> ": "
                  <> (T.intercalate ", " (S.toList unsupportedKeys))
                  <> ". Known keys: "
                  <> (T.intercalate ", " (S.toList supportedKeys))

newtype Location = Location {unLocation :: T.Text}
  deriving (Eq, Show)

unknownLocation :: Location
unknownLocation = Location "?:?"

data PluginCall = PluginCall
  { pc_pluginName :: PluginName,
    pc_location :: Location,
    pc_sectionName :: Maybe T.Text,
    pc_args :: ArgMap,
    pc_body :: T.Text
  }
  deriving (Eq, Show)

type PluginMap m = M.Map PluginName (AnyPluginConfig m)

data AnyPluginConfig action = forall state. AnyPluginConfig {pluginConfig :: PluginConfig state action}

data PluginConfig state action = PluginConfig
  { p_name :: PluginName,
    p_kind :: PluginKind,
    -- | Called in the conversion step .md ~~> .mdraw
    p_init :: action state,
    p_expand ::
      BuildConfig ->
      BuildArgs ->
      state ->
      PluginCall ->
      ExceptT T.Text action (T.Text, state),
    p_forAllCalls ::
      BuildConfig ->
      BuildArgs ->
      [PluginCall] ->
      ExceptT T.Text action ()
  }

instance Show (PluginConfig s a) where
  showsPrec p cfg =
    showParen (p > 10) $
      showString "PluginConfig "
        . showsPrec 11 (p_name cfg)
        . showString " "
        . showsPrec 11 (p_kind cfg)

instance Show (AnyPluginConfig a) where
  showsPrec p (AnyPluginConfig cfg) = showsPrec p cfg

anyPluginName :: AnyPluginConfig action -> PluginName
anyPluginName (AnyPluginConfig cfg) = p_name cfg
