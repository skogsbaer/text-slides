module Types where

import Development.Shake
import Safe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import Control.Monad.Trans.Except

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

data GenericBuildConfig m
    = BuildConfig
    { bc_buildDir :: FilePath
    , bc_pandoc :: FilePath
    , bc_plugins :: [PluginConfig m]
    }

type BuildConfig = GenericBuildConfig Action

newtype PluginName = PluginName { unPluginName :: T.Text }
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

castArgValue :: T.Text -> ArgValue -> T.Text -> (ArgValue -> Maybe a) -> Fail a
castArgValue k v ty fun =
    case fun v of
        Just x -> Right x
        Nothing -> Left $ T.pack ("Parameter " ++ show k ++ " should have type " ++ T.unpack ty)

type ArgMap = M.Map T.Text ArgValue

getRequiredValue :: T.Text -> ArgMap -> T.Text -> (ArgValue -> Maybe a) -> Fail a
getRequiredValue k m ty fun =
    case M.lookup k m of
        Nothing -> Left $ T.pack ("Missing required parameter " ++ show k)
        Just v -> castArgValue k v ty fun

getOptionalValue :: T.Text -> ArgMap -> T.Text -> (ArgValue -> Maybe a) -> Fail (Maybe a)
getOptionalValue k m ty fun =
    case M.lookup k m of
        Nothing -> Right Nothing
        Just v -> fmap Just $ castArgValue k v ty fun

getRequiredStringValue :: T.Text -> ArgMap -> Fail T.Text
getRequiredStringValue k m =
    getRequiredValue k m "String" asString

getOptionalStringValue :: T.Text -> ArgMap -> Fail (Maybe T.Text)
getOptionalStringValue k m =
    getOptionalValue k m "String" asString

getRequiredIntValue :: T.Text -> ArgMap -> Fail Int
getRequiredIntValue k m =
    getRequiredValue k m "Int" asInt

getOptionalIntValue :: T.Text -> ArgMap -> Fail (Maybe Int)
getOptionalIntValue k m =
    getOptionalValue k m "Int" asInt

getRequiredBoolValue :: T.Text -> ArgMap -> Fail Bool
getRequiredBoolValue k m =
    getRequiredValue k m "Bool" asBool

getOptionalBoolValue :: T.Text -> ArgMap -> Fail (Maybe Bool)
getOptionalBoolValue k m =
    getOptionalValue k m "Bool" asBool

data PluginCall
    = PluginCall
    { pc_pluginName :: PluginName
    , pc_location :: T.Text
    , pc_args :: (ArgMap)
    , pc_body :: T.Text
    }
    deriving (Eq, Show)

data PluginConfig action
    = PluginConfig
    { p_name :: PluginName
    , p_kind :: PluginKind
    , p_rules :: Rules ()
    , p_expand :: PluginCall -> ExceptT T.Text action T.Text
    }
