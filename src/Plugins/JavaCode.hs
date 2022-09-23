module Plugins.JavaCode (javaLangConfig) where

{-

Next steps:

- Implement test for java plugin
- Implement missing pieces of the java plugin
- Test the java plugin
- Make sure that the tests of the lectures AKI_Prog_Java and AdvancedProg are working

-}

import Control.Monad
-- import qualified Language.Java.Syntax as J

import Control.Monad.Extra
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Development.Shake (Action)
-- import Language.Java.Parser
import Language.Java.Syntax hiding (Location)
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
  If false, then the snippet requires the content of the next snippet to compile.
  Default: true

- clear:BOOL
  Starts a new group if true. Default: false

At most one of arguments method, body, or test can be given.

Processing:

The plugin processes the snippets sharing the same key in order of the list in the CodeMap.
Assume that the key is K. The plugin then performs the following steps on the snippets without
an explicit place: argument, i.e. the snippets in cf_here. Step 1 also applies
to snippets with an explicit place: argument

STEP 1: Snippets with rewrite:true are rewritten.

STEP 2: Snippets with append:true are appended to their predecessor. Snippets with
  standalone:false are prepended to their successor.

STEP 3: Each snippet with method:true or body:true or with a test argument are appened to
  the nearest predecessor without such an argument.

After these steps, we have a list of type [MergedSnippet] for each key K. The value
of ms_clear comes from the clear argument of the first snippet without
method:true, body:true or a test argument.
-}

data MergedSnippet = MergedSnippet
  { ms_clear :: Bool,
    ms_baseSnippets :: T.Text,
    ms_methodSnippets :: [T.Text],
    ms_bodySnippets :: [T.Text],
    ms_testSnippets :: [T.Text],
    ms_locations :: [Location]
  }
  deriving (Eq, Show)

{-
STEP 4: Group the snippets. Each merged snippet with a package P is said to start
  a new group. Also, each merged snippet with ms_clear = True starts a new
  group. Such a snippet must consist of toplevel code.

After this step, we have a list of type [SnippetGroup] for each key K.

The group ID is the group name, made unique if necessary by appending version strings
"_01", "_02" ... The group name is the package name if it's not the default package,
"default_pkg" otherwise.

-}

data PackageName
  = PackageDefault
  | PackageName T.Text -- not empty
  deriving (Eq, Show)

newtype GroupId = GroupId {unGroupId :: T.Text}
  deriving (Eq, Show)

data SnippetGroup = SnippetGroup
  { sg_package :: PackageName,
    sg_groupId :: GroupId,
    sg_snippets :: [MergedSnippet]
  }
  deriving (Eq, Show)

{-

STEP 5: The content of each merged snippet in each group is updated.
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

  Steps 4 and 5 require parsing of Java code.

After this step, we still have a list of type [SnippetGroup] for each key K, but with
updated content.
-}

{-
STEP 6: Transform a list [SnippetGroup] into a list [JSnippet].
  - The version is just the postion of the snippet in group.
  - The class name is determined as follows:
    * First public class
    * First non-public class
    If no main class is found, the name 'Main' is assumed.
-}
newtype ClassName = ClassName {unClassName :: T.Text}
  deriving (Eq, Show)

newtype Version = Version {unVersion :: Int}
  deriving (Eq, Show)

data JSnippet = JSnippet
  { js_baseSnippets :: T.Text,
    js_methodSnippets :: [T.Text],
    js_bodySnippets :: [T.Text],
    js_testSnippets :: [T.Text],
    js_locations :: [Location],
    js_version :: Version,
    js_package :: PackageName,
    js_group :: GroupId,
    js_mainClass :: ClassName,
    js_key :: CodeFilePath
  }
  deriving (Eq, Show)

{-

STEP 7: write each JSnippet into a file. Let V be the version, P the package, G the group,
  C the class name, and K the key. Then the snippet is written to K/G/V/P/C.java

  - If K is the default key, it is omitted.
  - If there is only one K, it is omitted.
  - If there is only one G, it is omitted.
  - If there is only one V, it is omitted.
  - If P is the default package, it is omitted.

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
append snips = do
  annotatedSnips <- annotateAppend snips
  loop Nothing annotatedSnips
  where
    annotateAppend :: [CodeSnippet] -> Fail [(CodeSnippet, Bool)] -- bool flag for append
    annotateAppend [] = pure []
    annotateAppend (x : xs) = do
      append <- getArg "append" x
      standalone <- getArg "standalone" x
      xs' <- annotateAppend xs
      case (standalone, xs') of
        (False, ((y, _) : rest)) -> pure $ (x, append) : (y, True) : rest
        (True, rest) -> pure $ (x, append) : rest
        (False, []) ->
          Left
            ( unLocation (cc_location x)
                <> ": standalone:false for last snippet"
            )
    getArg k x = fromMaybe False <$> getOptionalBoolValue (cc_location x) k (cc_args x)
    loop :: Maybe [CodeSnippet] -> [(CodeSnippet, Bool)] -> Fail [[CodeSnippet]]
    loop Nothing [] = pure [[]]
    loop Nothing (x : xs) = loop (Just [fst x]) xs
    loop (Just cur) [] = pure [cur]
    loop (Just cur) ((x, b) : xs) = do
      if not b
        then do
          rest <- loop (Just [x]) xs
          pure (cur : rest)
        else loop (Just (cur ++ [x])) xs

-- STEP 3

data SnippetKind
  = SnippetKindRegular
  | SnippetKindBody
  | SnippetKindMethod
  | SnippetKindTest T.Text
  deriving (Eq, Show)

emptyPreMergedSnippet :: PreMergedSnippet
emptyPreMergedSnippet = PreMergedSnippet [] [] [] []

addToPreMergedSnippet :: SnippetKind -> [CodeSnippet] -> PreMergedSnippet -> PreMergedSnippet
addToPreMergedSnippet k snips pms =
  case k of
    SnippetKindRegular -> pms {pms_baseSnippets = pms_baseSnippets pms ++ snips}
    SnippetKindBody -> pms {pms_bodySnippets = pms_bodySnippets pms ++ [snips]}
    SnippetKindMethod -> pms {pms_methodSnippets = pms_methodSnippets pms ++ [snips]}
    SnippetKindTest _ -> pms {pms_testSnippets = pms_testSnippets pms ++ [snips]}

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

data PreMergedSnippet = PreMergedSnippet
  { pms_baseSnippets :: [CodeSnippet],
    pms_methodSnippets :: [[CodeSnippet]],
    pms_bodySnippets :: [[CodeSnippet]],
    pms_testSnippets :: [[CodeSnippet]]
  }
  deriving (Eq, Show)

snippetsToText :: [CodeSnippet] -> T.Text
snippetsToText = mkCode javaLangConfig

merge :: [[CodeSnippet]] -> Fail [MergedSnippet]
merge xss = do
  pms <- loop Nothing xss
  mapM toMergedSnippet pms
  where
    toMergedSnippet :: PreMergedSnippet -> Fail MergedSnippet
    toMergedSnippet pms = do
      clear <-
        case pms_baseSnippets pms of
          (x : _) ->
            fromMaybe False <$> getOptionalBoolValue (cc_location x) "clear" (cc_args x)
          [] -> pure False
      pure $
        MergedSnippet
          { ms_clear = clear,
            ms_baseSnippets = snippetsToText (pms_baseSnippets pms),
            ms_methodSnippets = map snippetsToText (pms_methodSnippets pms),
            ms_bodySnippets = map snippetsToText (pms_bodySnippets pms),
            ms_testSnippets = map snippetsToText (pms_testSnippets pms),
            ms_locations = map cc_location (pms_baseSnippets pms)
          }
    loop :: Maybe PreMergedSnippet -> [[CodeSnippet]] -> Fail [PreMergedSnippet]
    loop Nothing [] = pure [emptyPreMergedSnippet]
    loop (Just ms) [] = pure [ms]
    loop ctx (ys : yss) =
      case ys of
        [] -> loop ctx yss
        (snip : _) -> do
          k <- snippetKind snip
          case k of
            SnippetKindRegular -> do
              let newCtx = addToPreMergedSnippet k ys emptyPreMergedSnippet
              rest <- loop (Just newCtx) yss
              case ctx of
                Nothing -> pure rest
                Just ms -> pure (ms : rest)
            _ -> do
              let ms = fromMaybe emptyPreMergedSnippet ctx
                  newCtx = addToPreMergedSnippet k ys ms
              loop (Just newCtx) yss

-- STEP 4

parseJava :: T.Text -> Maybe CompilationUnit
parseJava _code = undefined

{-
  -- should we fail here? Swallowing errors is ugly
  case parserWithMode ParseShallow compilationUnit "<input>" (T.unpack code) of
    Left err -> putStrLn (fp ++ ": ERROR: " ++ show err)
    Right x -> return ()
-}

getPackageName :: T.Text -> Fail (Maybe PackageName)
getPackageName = undefined

-- Example: ["a", "b", "c", "b"] ~~> ["a", "b_01", "c", "b_02"]
addVersionsIfNecessary :: [T.Text] -> [T.Text]
addVersionsIfNecessary l = l

groupSnippets :: [MergedSnippet] -> Fail [SnippetGroup]
groupSnippets mss = do
  groups <- loop Nothing mss
  let versionedIds = addVersionsIfNecessary (map (unGroupId . sg_groupId) groups)
  pure $ flip map (zip versionedIds groups) $ \(v, g) -> g {sg_groupId = GroupId v}
  where
    groupIdFromPkgName pkgName =
      GroupId $
        case pkgName of
          PackageDefault -> "default_pkg"
          PackageName p -> p
    loop :: Maybe SnippetGroup -> [MergedSnippet] -> Fail [SnippetGroup]
    loop Nothing [] = pure []
    loop Nothing (x : xs) = do
      mPkgName <- getPackageName (ms_baseSnippets x)
      let pkgName = fromMaybe PackageDefault mPkgName
          group = SnippetGroup pkgName (groupIdFromPkgName pkgName) [x]
      loop (Just group) xs
    loop (Just group) [] = pure [group]
    loop (Just group) (x : xs) = do
      mPkgName <- do
        mp <- getPackageName (ms_baseSnippets x)
        case mp of
          Just _ -> pure mp
          _ ->
            case ms_clear x of
              True -> pure (Just PackageDefault)
              False -> pure Nothing
      case mPkgName of
        Just pkgName -> do
          -- start new group
          let newGroup = SnippetGroup pkgName (groupIdFromPkgName pkgName) [x]
          rest <- loop (Just newGroup) xs
          pure (group : rest)
        Nothing ->
          loop (Just (group {sg_snippets = sg_snippets group ++ [x]})) xs

-- STEP 5
_parseMembers :: T.Text -> Maybe [MemberDecl]
_parseMembers = undefined

parseJavaOrMembers :: T.Text -> Maybe (Either CompilationUnit [MemberDecl])
parseJavaOrMembers = undefined

concatCode :: T.Text -> T.Text -> T.Text
concatCode = undefined -- ensure one blank link

mergeCu :: MergedSnippet -> CompilationUnit -> MergedSnippet -> CompilationUnit -> MergedSnippet
mergeCu _prevSnip _prevCu _snip _cu = undefined

mergeMembers :: MergedSnippet -> CompilationUnit -> MergedSnippet -> [MemberDecl] -> MergedSnippet
mergeMembers _prevSnip _prevCu _snip _methods = undefined

updateSnippetContent :: MergedSnippet -> MergedSnippet -> Fail MergedSnippet
updateSnippetContent prevSnip snip = do
  let mPrevCu = parseJava (ms_baseSnippets prevSnip)
      mThisParsed = parseJavaOrMembers (ms_baseSnippets snip)
  case (mPrevCu, mThisParsed) of
    (Just prevCu, Just (Left thisCu)) ->
      pure $ mergeCu prevSnip prevCu snip thisCu
    (Just prevCu, Just (Right thisMethods)) ->
      pure $ mergeMembers prevSnip prevCu snip thisMethods
    _ ->
      pure $
        snip
          { ms_baseSnippets =
              ms_baseSnippets prevSnip `concatCode` ms_baseSnippets snip,
            ms_locations =
              ms_locations prevSnip ++ ms_locations snip
          }

updateGroup :: SnippetGroup -> Fail SnippetGroup
updateGroup sg = do
  newSnippets <-
    case sg_snippets sg of
      [] -> pure []
      [x] -> pure [x]
      (x : xs) -> do
        xs' <- loop x xs
        pure (x : xs')
  pure $ sg {sg_snippets = newSnippets}
  where
    loop _ [] = pure []
    loop prev (x : xs) = do
      newX <- updateSnippetContent prev x
      rest <- loop newX xs
      pure (newX : rest)

-- STEP 6
getMainClassName :: T.Text -> Fail (Maybe ClassName)
getMainClassName = undefined

_classDeclIdent :: ClassDecl -> Ident
_classDeclIdent d =
  case d of
    ClassDecl _ _ className _ _ _ _ -> className
    RecordDecl _ _ recName _ _ _ _ -> recName
    EnumDecl _ _ enumName _ _ -> enumName

_identToText :: Ident -> T.Text
_identToText = undefined

_nameToText :: Name -> T.Text
_nameToText = undefined

snippetGroupToJSnippets :: CodeFilePath -> SnippetGroup -> Fail [JSnippet]
snippetGroupToJSnippets key group =
  mapM toJSnippet (zip [1 ..] (sg_snippets group))
  where
    toJSnippet (v, ms) = do
      mClsName <- getMainClassName (ms_baseSnippets ms)
      pure $
        JSnippet
          { js_baseSnippets = ms_baseSnippets ms,
            js_methodSnippets = ms_methodSnippets ms,
            js_bodySnippets = ms_bodySnippets ms,
            js_testSnippets = ms_testSnippets ms,
            js_locations = ms_locations ms,
            js_version = Version v,
            js_package = sg_package group,
            js_group = sg_groupId group,
            js_mainClass = fromMaybe (ClassName "Main") mClsName,
            js_key = key
          }

-- Orchestration

processSnippets :: CodeFilePath -> [CodeSnippet] -> Fail [JSnippet]
processSnippets key snippets = do
  -- Step 1
  snippets <- mapM rewrite snippets
  -- Step 2&3
  merged <- append snippets >>= merge
  -- Step 4
  groups <- groupSnippets merged
  -- Step 5
  newGroups <- mapM updateGroup groups
  -- Step 6
  concatMapM (snippetGroupToJSnippets key) newGroups

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

outputJSnippet ::
  BuildArgs ->
  T.Text ->
  [CodeSnippet] ->
  JSnippet ->
  [CodeSnippet] ->
  [JSnippet] ->
  S.Set CodeFilePath ->
  Action ()
outputJSnippet _buildArgs header start snip end allSnippets allKeys = do
  let code = jsnippetCode start snip end
      key =
        case js_key snip of
          CodeFilePathDefault -> Nothing
          CodeFilePathCustom fp ->
            if S.size allKeys > 2
              then Nothing
              else Just fp
      groupId =
        if moreThanOne js_group
          then Just (T.unpack (unGroupId $ js_group snip))
          else Nothing
      version =
        if moreThanOne (\x -> (js_group x, js_version x))
          then
            let v = unVersion (js_version snip)
                versionDir = "v" ++ (if v < 10 then "0" else "") ++ show v
             in Just versionDir
          else Nothing
      pkgName =
        case js_package snip of
          PackageDefault -> Nothing
          PackageName p -> Just (T.unpack p)
      dir =
        concat $ L.intersperse "/" $ catMaybes [key, groupId, version, pkgName]
      file = dir </> T.unpack (unClassName (js_mainClass snip)) <.> "java"
  note ("Generating " ++ file)
  myWriteFile file (header <> code)
  where
    moreThanOne extract =
      let loop [] = False
          loop (x : xs) = extract x /= extract snip || loop xs
       in loop allSnippets

processCodeMap ::
  BuildConfig ->
  BuildArgs ->
  LangConfig ->
  T.Text ->
  CodeMap ->
  [CodeSnippet] ->
  Action ()
processCodeMap _buildCfg buildArgs _langCfg header cm allSnippets = do
  forM_ (M.toList cm) $ \(k, ccf) -> do
    jSnippets <- failInM $ processSnippets k (ccf_here ccf)
    forM_ jSnippets $ \snip ->
      outputJSnippet
        buildArgs
        header
        (ccf_atStart ccf)
        snip
        (ccf_atEnd ccf)
        jSnippets
        (M.keysSet cm)
  let fullCode = header <> snippetsToText allSnippets
      file = takeBaseName (ba_inputFile buildArgs) <.> "java"
  myWriteFile file fullCode

javaLangConfig :: LangConfig
javaLangConfig =
  (mkLangConfig "java" ".java" "// " Nothing processCodeMap)
    { lc_extraArgs = ["method", "body", "test", "append", "standalone"]
    }
