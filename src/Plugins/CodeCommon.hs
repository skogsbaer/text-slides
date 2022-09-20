module Plugins.CodeCommon where

import Data.Functor.Identity
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Development.Shake (Action)
import Types

-- The text wrapped in the Code newtype may contain directives
-- such as `# ~~~hide`.
data Code = Code
  { c_payload :: T.Text
  }
  deriving (Eq, Show)

data CodeSnippet = CodeSnippet
  { cc_code :: Code,
    cc_sectionName :: Maybe T.Text,
    cc_location :: Location,
    cc_args :: ArgMap
  }
  deriving (Eq, Show)

data CodeSnippetFile = CodeSnippetFile
  { ccf_atStart :: [CodeSnippet],
    ccf_here :: [CodeSnippet],
    ccf_atEnd :: [CodeSnippet]
  }
  deriving (Show)

data CodeFilePath
  = CodeFilePathCustom FilePath -- explicit file:NAME argument
  | CodeFilePathDefault
  deriving (Eq, Ord, Show)

type CodeMap = M.Map CodeFilePath CodeSnippetFile

applyToCodeSnippets ::
  ([CodeSnippet] -> [CodeSnippet]) ->
  CodeSnippetFile ->
  CodeSnippetFile
applyToCodeSnippets fun x =
  runIdentity $ applyToCodeSnippetsM (return . fun) x

applyToCodeSnippetsM ::
  Monad m =>
  ([CodeSnippet] -> m [CodeSnippet]) ->
  CodeSnippetFile ->
  m CodeSnippetFile
applyToCodeSnippetsM fun x = do
  start <- fun (ccf_atStart x)
  end <- fun (ccf_atEnd x)
  here <- fun (ccf_here x)
  return $
    CodeSnippetFile
      { ccf_atStart = start,
        ccf_here = here,
        ccf_atEnd = end
      }

applyToCodeSnippet ::
  (CodeSnippet -> CodeSnippet) ->
  CodeSnippetFile ->
  CodeSnippetFile
applyToCodeSnippet fun = applyToCodeSnippets (map fun)

applyToCodeSnippetM ::
  Monad m =>
  (CodeSnippet -> m CodeSnippet) ->
  CodeSnippetFile ->
  m CodeSnippetFile
applyToCodeSnippetM fun = applyToCodeSnippetsM (mapM fun)

type ProcessCodeMap = BuildConfig -> BuildArgs -> LangConfig -> T.Text -> CodeMap -> Action ()

data LangConfig = LangConfig
  { lc_name :: T.Text,
    lc_fileExt :: String,
    lc_commentStart :: T.Text,
    lc_commentEnd :: Maybe T.Text,
    lc_extraArgs :: [T.Text],
    lc_processCodeMap :: ProcessCodeMap
  }

data CodeExtract = CodeExtractFile | CodeExctractPresentation

extractCode :: LangConfig -> CodeExtract -> Code -> T.Text
extractCode lcfg mode (Code t) =
  let lines = T.lines t
      newLines = loop True lines []
   in T.unlines newLines
  where
    loop _ [] acc = reverse acc
    loop show (l : ls) acc =
      case parseShowHide l of
        Nothing ->
          loop show ls (if show then l : acc else acc)
        Just parsedShow ->
          let newShow =
                case mode of
                  CodeExtractFile -> True
                  CodeExctractPresentation -> parsedShow
           in loop newShow ls acc
    parseShowHide (T.stripEnd -> t) = do
      t <- T.stripPrefix (lc_commentStart lcfg) t
      t <- T.stripSuffix (fromMaybe "" (lc_commentEnd lcfg)) t
      case T.strip t of
        "~~~hide" -> pure False
        "~~~show" -> pure True
        _ -> Nothing

mkCode :: LangConfig -> [CodeSnippet] -> T.Text
mkCode langCfg code =
  let groupedBySectionName =
        L.groupBy (\x y -> cc_sectionName x == cc_sectionName y) code
      newCode =
        T.unlines $
          flip concatMap groupedBySectionName $ \ccs ->
            case ccs of
              [] -> []
              (cc : _) ->
                ( case cc_sectionName cc of
                    Just x ->
                      let line = T.replicate (T.length x) "-"
                       in ["", "", cmt line, cmt x, cmt line]
                    Nothing -> []
                )
                  ++ map getCode ccs
   in newCode
  where
    getCode = extractCode langCfg CodeExtractFile . cc_code
    cmt = lineComment langCfg

lineComment :: LangConfig -> T.Text -> T.Text
lineComment cfg t = lc_commentStart cfg <> t <> fromMaybe "" (lc_commentEnd cfg)

mkLangConfig :: T.Text -> String -> T.Text -> Maybe T.Text -> ProcessCodeMap -> LangConfig
mkLangConfig name ext commentStart commentEnd process =
  LangConfig
    { lc_name = name,
      lc_fileExt = ext,
      lc_commentStart = commentStart,
      lc_commentEnd = commentEnd,
      lc_extraArgs = [],
      lc_processCodeMap = process
    }
