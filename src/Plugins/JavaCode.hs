module Plugins.JavaCode (javaLangConfig) where

{-

Next steps:

- Implement test for java plugin
- Implement missing pieces of the java plugin
- Test the java plugin
- Make sure that the tests of the lectures AKI_Prog_Java and AdvancedProg are working

-}

import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Development.Shake (Action)
import Language.Java.Parser
import Language.Java.Syntax hiding (Location)
-- import qualified Language.Java.Syntax as J
import Logging (note)
import Plugins.CodeCommon
import System.FilePath
import Types
import Utils

{-
The following text describes how code snippets are processed by the java code plugin.

Terminology:

- A code snippet is a code fragment, represented "~~~java\nCODE\n~~~" in markdown
- The key of a code snippet is its key in the CodeMap (either CodeFilePathDefault or given by
  the file: argument)

Arguments:

The Java code plugin supports the following extra arguments for each code snippet.

- method:BOOL
  Assumes that the code is a method. Places the code inside class __CodeContainer.
  Default: false

- body:BOOL
  Assumes that the code is a method body.  Places the code inside some fresh method of
  class __CodeContainer.
  Default: false

- test:NAME
  Defines a test named NAME with the code as the test's body. Places the code inside some fresh
  method of class __CodeContainer.

- rewrite:BOOL
  Replaces { ... } with { return X; } where X is a default value for the return type of the method.
  Default: false

- standalone:BOOL
  If false, then the snippet requires the content of a later snippet to compile.
  Default: true

At most one of arguments method, body, or test can be given.

Processing:

The plugin processes the snippets sharing the same key in order of the list in the CodeMap.
Assume that the key is K. The plugin then performs the following steps on the snippets without
an explicit place: argument, i.e. the snippets in cf_here. Step 1 also applies
to snippets with an explicit place: argument

STEP 1: Snippets with rewrite:true are rewritten.

STEP 2: Snippets with append:true are appended to their predecessor.

STEP 3: Each snippet with method:true or body:true or with a test argument are appened to
  the nearest predecessor without such an argument.

After these steps, we have a list of type [MergedSnippet] for each key K.
-}

data MergedSnippet = MergedSnippet
  { ms_baseSnippets :: [CodeSnippet],
    ms_methodSnippets :: [[CodeSnippet]],
    ms_bodySnippets :: [[CodeSnippet]],
    ms_testSnippets :: [[CodeSnippet]]
  }
  deriving (Eq, Show)

{-
STEP 4: Each (remaining) snippet gets a version number V. This is just the position in the list,
  starting at 1. The goal is now to patch each snippet so that in compiles in isolation.
  Patching a snippet enriches its content with content from the preceding snippet.

STEP 5: For each snippet, the plugin first tries to extract the package name P and the name of
  the main class C from the snippet. The main class is determined as follows (first match is used):
  * First public class
  * First non-public class

  If no main class is found, the name 'Main' is assumed.

STEP 6: Each snippets gets an ID, determined as follows:
  1. If the snippets has P and C, the ID is P/C.
  2. If the snippet has P but no C, the ID is P.
  2. If the snippets does not have P and C, the ID is
     a. the ID of the preceeding snippets if there is such a snippet.
     b. the key of the snippet otherwise.
  In cases 1 and 2b, the snippet is said to have an explicit ID, in case 2a the ID is implicit.

After step 6, we have a list of type [JSnippet] for each key K.
-}

data JSnippetId
  = JSnippetIdPkgClass T.Text T.Text
  | JSnippetIdPkg T.Text
  | JSnippetIdKey CodeFilePath
  deriving (Eq, Show)

data JSnippetIdKind = JSnippetExplicitId | JSnippetImplictId
  deriving (Eq, Show)

newtype Version = Version {unVersion :: Int}
  deriving (Eq, Show)

data JSnippet = JSnippet
  { js_baseSnippets :: T.Text,
    js_methodSnippets :: [T.Text],
    js_bodySnippets :: [T.Text],
    js_testSnippets :: [T.Text],
    js_locations :: [Location],
    js_standalone :: Bool,
    js_version :: Version,
    js_id :: JSnippetId,
    js_idKind :: JSnippetIdKind,
    js_key :: CodeFilePath
  }
  deriving (Eq, Show)

{-
STEP 7: The content of each snippet with an implicit ID is updated. Note that such a snippet must
  have a preceding snippet.
  Snippets with lower versions numbers are handled first, so that the updated content is used when
  handling higher versions.
  * CASE nested, i.e. the code contains only methods. The plugin tries to find methods with
    the same name in the toplevel declarations of the preceding snippet.
    - If it finds such a method, the new content of the snippet is the content of the preceding
      snippet with the method being updated.
    - If no such method exists, the new content of the snippet is the content of the preceding
      snippet with the method being appended to the content of the main class of the preceding
      snippet.
  * CASE toplevel.
    The new content of the snippet is the content of the preceding snippet such that
     - declarations that already exist in the preceding snippet are replaced,
     - declarations that do not existing in the preceding snippet are appended.
  Steps 6 and 7 requires parsing of Java code.

STEP 8: Each snippet is written to some file. The snippets with place:"atStart" and
  place:"atEnd" for key K are prepended/appended to each file. Method, test and bodies snippets
  are place inside class __CodeContainer in the same file.

  The file name is determined as follows:

  - If ID is of the form P/C, the filename is P/V/C.java
  - If ID is of the form P, the filename is P/V/Main.java
  - If the ID is of the form K and K is the default, the filename is default/V/_Main.java.
  - If the ID is of the form K and K is not the default, the filename is K/V/_Main.java.

In addition, all snippets are written unchanged to file INPUT_FILE.java (to enable copy&paste).

-}

-- STEP 1
rewrite :: CodeSnippet -> Fail CodeSnippet
rewrite snip = do
  b <- fromMaybe False <$> getOptionalBoolValue (cc_location snip) "rewrite" (cc_args snip)
  if not b
    then pure snip
    else
      let code = c_payload (cc_code snip)
          newCode = T.unlines (map rewriteLine (T.lines code))
       in pure $ snip {cc_code = Code newCode}
  where
    methodReturnType line =
      let ws = T.words (T.strip line)
          isKw x = x `elem` ["public", "private", "protected", "final", "abstract", "static"]
       in case filter (not . isKw) ws of
            [] -> Nothing
            l ->
              case T.words (T.pack (dropTyArgs 0 (T.unpack (T.unwords l)))) of
                [] -> Nothing
                (x : _) -> Just x
    dropTyArgs i ('<' : rest) = dropTyArgs (i + 1) rest
    dropTyArgs i ('>' : rest) = dropTyArgs (i -1) rest
    dropTyArgs 0 s = s
    dropTyArgs i (_ : rest) = dropTyArgs i rest
    dropTyArgs _ [] = []
    rewriteLine line =
      case methodReturnType line of
        Nothing -> line
        Just ty ->
          let repl =
                case ty of
                  "void" -> "{ }"
                  "boolean" -> "{ return false; }"
                  "int" -> "{ return 0; }"
                  "short" -> "{ return 0; }"
                  "byte" -> "{ return 0; }"
                  "long" -> "{ return 0L; }"
                  "double" -> "{ return 0.0; }"
                  "float" -> "{ return 0.0f; }"
                  "char" -> "{ return '\\0'; }"
                  _ -> "{ return null; }"
           in T.replace "{ ... }" repl (T.replace "{...}" repl line)

-- STEP 2
append :: [CodeSnippet] -> Fail [[CodeSnippet]]
append snips = loop Nothing snips
  where
    loop :: Maybe [CodeSnippet] -> [CodeSnippet] -> Fail [[CodeSnippet]]
    loop Nothing [] = pure [[]]
    loop Nothing (x : xs) = loop (Just [x]) xs
    loop (Just cur) [] = pure [cur]
    loop (Just cur) (x : xs) = do
      b <- fromMaybe False <$> getOptionalBoolValue (cc_location x) "append" (cc_args x)
      if not b
        then do
          rest <- loop (Just [x]) xs
          pure (cur : rest)
        else loop (Just (cur ++ [x])) xs -- FIXME: check if append is ok (method, body, test must match)

-- STEP 3

data SnippetKind
  = SnippetKindRegular
  | SnippetKindBody
  | SnippetKindMethod
  | SnippetKindTest T.Text
  deriving (Eq, Show)

emptyMergedSnippet :: MergedSnippet
emptyMergedSnippet = MergedSnippet [] [] [] []

addToMergedSnippet :: SnippetKind -> [CodeSnippet] -> MergedSnippet -> MergedSnippet
addToMergedSnippet k snips ms =
  case k of
    SnippetKindRegular -> ms {ms_baseSnippets = ms_baseSnippets ms ++ snips}
    SnippetKindBody -> ms {ms_bodySnippets = ms_bodySnippets ms ++ [snips]}
    SnippetKindMethod -> ms {ms_methodSnippets = ms_methodSnippets ms ++ [snips]}
    SnippetKindTest _ -> ms {ms_testSnippets = ms_testSnippets ms ++ [snips]}

snippetKind :: CodeSnippet -> Fail SnippetKind
snippetKind cs = do
  m <- get "method"
  b <- get "body"
  t <- getOptionalStringValue loc "test" (cc_args cs)
  case (m, b, t) of
    (True, False, Nothing) -> pure SnippetKindMethod
    (False, True, Nothing) -> pure SnippetKindBody
    (False, False, Just t) -> pure (SnippetKindTest t)
    (False, False, Nothing) -> pure SnippetKindRegular
    _ -> Left "Can have only one of the arguments method, body, and test"
  where
    loc = cc_location cs
    get k = fromMaybe False <$> getOptionalBoolValue loc k (cc_args cs)

merge :: [[CodeSnippet]] -> Fail [MergedSnippet]
merge xss = loop Nothing xss
  where
    loop :: Maybe MergedSnippet -> [[CodeSnippet]] -> Fail [MergedSnippet]
    loop Nothing [] = pure [emptyMergedSnippet]
    loop (Just ms) [] = pure [ms]
    loop ctx (ys : yss) =
      case ys of
        [] -> loop ctx yss
        (snip : _) -> do
          k <- snippetKind snip
          case k of
            SnippetKindRegular -> do
              let newCtx = addToMergedSnippet k ys emptyMergedSnippet
              rest <- loop (Just newCtx) yss
              case ctx of
                Nothing -> pure rest
                Just ms -> pure (ms : rest)
            _ -> do
              let ms = fromMaybe emptyMergedSnippet ctx
                  newCtx = addToMergedSnippet k ys ms
              loop (Just newCtx) yss

-- STEP 4 - 6

snippetsToText :: [CodeSnippet] -> T.Text
snippetsToText = mkCode javaLangConfig

parseJava :: T.Text -> Maybe CompilationUnit
parseJava code =
  -- should we fail here? Swallowing errors is ugly
  case parserWithMode ParseShallow compilationUnit "<input>" (T.unpack code) of
    Left err -> putStrLn (fp ++ ": ERROR: " ++ show err)
    Right x -> return ()

_parseMembers :: T.Text -> Maybe [MemberDecl]
_parseMembers = undefined

parseJavaOrMembers :: T.Text -> Maybe (Either CompilationUnit [MemberDecl])
parseJavaOrMembers = undefined

findMainClass :: [TypeDecl] -> Maybe ClassDecl
findMainClass = undefined

classDeclIdent :: ClassDecl -> Ident
classDeclIdent d =
  case d of
    ClassDecl _ _ className _ _ _ _ -> className
    RecordDecl _ _ recName _ _ _ _ -> recName
    EnumDecl _ _ enumName _ _ -> enumName

identToText :: Ident -> T.Text
identToText = undefined

nameToText :: Name -> T.Text
nameToText = undefined

mergedSnippetToJSnippet ::
  Version -> Maybe JSnippetId -> CodeFilePath -> MergedSnippet -> Fail JSnippet
mergedSnippetToJSnippet version mPrevId key ms = do
  let baseCode = snippetsToText (ms_baseSnippets ms)
      methodCode = map snippetsToText (ms_methodSnippets ms)
      bodyCode = map snippetsToText (ms_bodySnippets ms)
      testCode = map snippetsToText (ms_testSnippets ms)
  let mPkgId =
        case parseJava baseCode of
          Just (CompilationUnit (Just (PackageDecl pkg)) _ decls) -> do
            case fmap (identToText . classDeclIdent) (findMainClass decls) of
              Just className -> Just $ JSnippetIdPkgClass (nameToText pkg) className
              Nothing -> Just $ JSnippetIdPkg (nameToText pkg)
          _ -> Nothing
  let (idKind, id) =
        case mPkgId of
          Just i -> (JSnippetExplicitId, i)
          Nothing ->
            case mPrevId of
              Just prevId -> (JSnippetImplictId, prevId)
              _ -> (JSnippetExplicitId, JSnippetIdKey key)
  standalone <-
    case ms_baseSnippets ms of
      [] -> pure False
      (snip : _) -> do
        fromMaybe False <$> getOptionalBoolValue (cc_location snip) "standalone" (cc_args snip)
  pure $
    JSnippet
      { js_baseSnippets = baseCode,
        js_locations = map cc_location (ms_baseSnippets ms),
        js_methodSnippets = methodCode,
        js_bodySnippets = bodyCode,
        js_testSnippets = testCode,
        js_standalone = standalone,
        js_version = version,
        js_id = id,
        js_idKind = idKind,
        js_key = key
      }

mergedSnippetsToJSnippets :: CodeFilePath -> [MergedSnippet] -> Fail [JSnippet]
mergedSnippetsToJSnippets key snippets = loop 1 Nothing snippets
  where
    loop _ _ [] = pure []
    loop i mPrevId (x : xs) = do
      snip <- mergedSnippetToJSnippet (Version i) mPrevId key x
      rest <- loop (i + 1) (Just (js_id snip)) xs
      pure (snip : rest)

-- STEP 7

concatCode :: T.Text -> T.Text -> T.Text
concatCode = undefined -- ensure one blank link

formatLocations :: [Location] -> T.Text
formatLocations = undefined

mergeCu :: JSnippet -> CompilationUnit -> JSnippet -> CompilationUnit -> JSnippet
mergeCu _prevSnip _prevCu _snip _cu = undefined

mergeMembers :: JSnippet -> CompilationUnit -> JSnippet -> [MemberDecl] -> JSnippet
mergeMembers _prevSnip _prevCu _snip _methods = undefined

updateSnippetContent :: Maybe JSnippet -> JSnippet -> Fail JSnippet
updateSnippetContent mPrevSnip snip =
  case (mPrevSnip, js_idKind snip) of
    (Just prevSnip, JSnippetImplictId) -> pure $ merge prevSnip snip
    (Nothing, JSnippetImplictId) ->
      Left
        ( "Snippet with implicit ID but without a preceding snippet. Locations: "
            <> formatLocations (js_locations snip)
        )
    _ -> pure snip
  where
    merge prevSnip snip = do
      let mPrevCu = parseJava (js_baseSnippets prevSnip)
          mThisParsed = parseJavaOrMembers (js_baseSnippets snip)
      case (mPrevCu, mThisParsed) of
        (Just prevCu, Just (Left thisCu)) ->
          mergeCu prevSnip prevCu snip thisCu
        (Just prevCu, Just (Right thisMethods)) ->
          mergeMembers prevSnip prevCu snip thisMethods
        _ ->
          snip
            { js_baseSnippets =
                js_baseSnippets prevSnip `concatCode` js_baseSnippets snip,
              js_locations =
                js_locations prevSnip ++ js_locations snip
            }

finalizeSnippets :: [JSnippet] -> Fail [JSnippet]
finalizeSnippets snippets = loop Nothing snippets
  where
    loop _ [] = pure []
    loop prev (x : xs) = do
      y <- updateSnippetContent prev x
      rest <- loop (Just y) xs
      if js_standalone y
        then pure (y : rest)
        else pure rest

-- Orchestration

processSnippets :: CodeFilePath -> [CodeSnippet] -> Fail [JSnippet]
processSnippets key snippets = do
  -- Step 1
  snippets <- mapM rewrite snippets
  -- Step 2&3
  merged <- append snippets >>= merge
  -- Step 4&5&6
  jSnippets <- mergedSnippetsToJSnippets key merged
  -- Step 7
  finalizeSnippets jSnippets

jsnippetCode :: [CodeSnippet] -> JSnippet -> [CodeSnippet] -> T.Text
jsnippetCode start snip end =
  let startCode = snippetsToText start
      endCode = snippetsToText end
      snipCode = js_baseSnippets snip
   in startCode `concatCode` snipCode `concatCode` endCode
        `concatCode` container (js_bodySnippets snip) (js_methodSnippets snip) (js_testSnippets snip)
  where
    idFromCode code = unHash (md5OfText code)
    container bodies methods tests =
      let methodsForBodies = flip map bodies $ \code ->
            "public static void __body_" <> idFromCode code <> "() throws Exception {\n"
              <> code
              <> "\n}"
          methodsForTests = flip map tests $ \code ->
            "@Test public void __body_" <> idFromCode code <> "() throws Exception {\n"
              <> code
              <> "\n}"
          allMethods = methods ++ methodsForBodies ++ methodsForTests
       in if null allMethods
            then ""
            else
              let code = T.concat (L.intersperse "\n\n" allMethods)
               in "class __CodeContainer {\n" <> code <> "\n}"

outputJSnippet :: BuildArgs -> T.Text -> [CodeSnippet] -> JSnippet -> [CodeSnippet] -> Action ()
outputJSnippet _buildArgs header start snip end = do
  let code = jsnippetCode start snip end
      version = unVersion (js_version snip)
      versionDir =
        "v" ++ (if version < 10 then "0" else "") ++ show version
      file =
        case js_id snip of
          JSnippetIdPkgClass (T.unpack -> pkg) (T.unpack -> cls) ->
            pkg </> versionDir </> (cls <.> "java")
          JSnippetIdPkg (T.unpack -> pkg) -> pkg </> versionDir </> ("Main.java")
          JSnippetIdKey CodeFilePathDefault -> "default" </> versionDir </> "Main.java"
          JSnippetIdKey (CodeFilePathCustom fp) -> "default" </> versionDir </> fp
  note ("Generating " ++ file)
  myWriteFile file (header <> code)

processCodeMap ::
  BuildConfig ->
  BuildArgs ->
  LangConfig ->
  T.Text ->
  CodeMap ->
  [CodeSnippet] ->
  Action ()
processCodeMap _buildCfg buildArgs _langCfg header cm snippets = do
  forM_ (M.toList cm) $ \(k, ccf) -> do
    jSnippets <- failInM $ processSnippets k (ccf_here ccf)
    forM_ jSnippets $ \snip ->
      outputJSnippet buildArgs header (ccf_atStart ccf) snip (ccf_atEnd ccf)
  let fullCode = header <> snippetsToText snippets
      file = takeBaseName (ba_inputFile buildArgs) <.> "java"
  myWriteFile file fullCode

javaLangConfig :: LangConfig
javaLangConfig =
  (mkLangConfig "java" ".java" "// " Nothing processCodeMap)
    { lc_extraArgs = ["method", "body", "test", "append", "standalone"]
    }
