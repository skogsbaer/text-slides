module Plugins.JavaCode
  ( javaLangConfig,
    -- for testing
    Decl (..),
    addVersionsIfNecessary,
    getCode,
    CodePatch(..), patchCode,
    locationToIndex,
  )
where

import Control.Monad
import Control.Monad.Extra
import Data.Char (isSpace)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Development.Shake (Action)
import qualified Language.Java.Lexer as JavaLexer
import Language.Java.Parser
import Language.Java.Syntax hiding (Decl, Location)
import qualified Language.Java.Syntax as JavaSyntax
import Plugins.CodeCommon
import System.FilePath
import qualified Text.Parsec as Parsec
import Types
import Utils
import Logging

type JLocation = JavaSyntax.Location

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

- body:BOOL or body:NAME
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

- contextStart:CODE
  Emit CODE before the code of the snippet
- contextEnd:CODE
  Emit CODE after the code of the snippet
- context:NAME
  Emits code before/after the snippet according to some predefined context. Predefined contexts:
  * "main": puts the code inside the main-method

- class:NAME
  Specifies to which class the code of the snippet is added/updated. If added to the first
  snippet of a group, the argument specifies the default class where code of other snippets
  should be appended/updated. If added to a non-toplevel snippet, it defines the class
  for this very snippet.

At most one of arguments method, body, or test can be given.

Processing:

The plugin processes the snippets sharing the same key in order of the list in the CodeMap.
Assume that the key is K. The plugin then performs the following steps on the snippets without
an explicit place: argument, i.e. the snippets in cf_here. Step 1 also applies
to snippets with an explicit place: argument

STEP 1: Snippets with rewrite:true and contexts are rewritten.

STEP 2: Snippets with append:true are appended to their predecessor. Snippets with
  standalone:false are prepended to their successor.

STEP 3: Each snippet with method:true or body:true or with a test argument are appened to
  the nearest predecessor without such an argument.

After these steps, we have a list of type [MergedSnippet] for each key K. The value
of ms_clear comes from the clear argument of the first snippet without
method:true, body:true or a test argument.
-}

newtype TestName = TestName { unTestName :: T.Text }
  deriving (Eq, Show)

data JTest = JTest
  { jt_name :: Maybe TestName
  , jt_code :: T.Text
  }
  deriving (Eq, Show)

newtype BodyName = BodyName { unBodyName :: T.Text }
  deriving (Eq, Show)

data JBody = JBody
  { jb_name :: Maybe BodyName
  , jb_code :: T.Text
  }
  deriving (Eq, Show)

newtype ClassName = ClassName {unClassName :: T.Text}
  deriving (Eq, Show)

data MergedSnippet = MergedSnippet
  { ms_clear :: Bool,
    ms_baseSnippets :: T.Text,
    ms_methodSnippets :: [T.Text],
    ms_bodySnippets :: [JBody],
    ms_testSnippets :: [JTest],
    ms_locations :: [Location],
    ms_class :: Maybe ClassName
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

The main class of a SnippetGroup is the main class of the first MergedSnippet (if it exists).
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
    sg_snippets :: [MergedSnippet],
    sg_mainClass :: Maybe ClassName
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
      snippet. The main class is either explicitly provided by the snippet itself (through argument
      class:NAME, stored in field ms_class) or by the snippet group (class:NAME argument
      of first snippet in the group, stored in field sg_mainClass), or it is determined
      as explained below.
  * CASE toplevel.
    The new content of the snippet is the content of the preceding snippet such that
     - declarations that already exist in the preceding snippet are replaced,
     - declarations that do not existing in the preceding snippet are appended.

  Steps 4 and 5 require parsing of Java code.

After this step, we still have a list of type [SnippetGroup] for each key K, but with
updated content.

The default main class is the first public class (if any exists) or the first non-public
class (otherwise).
-}

{-
STEP 6: Transform a list [SnippetGroup] into a list [JSnippet].
  - The version is just the postion of the snippet in group.
  - js_mainClass is the name of the main class. The main class is the mainClass of the SnippetGroup
    (if there exists one), or the default main class (see above), or 'Main'.
-}

newtype Version = Version {unVersion :: Int}
  deriving (Eq, Show)

data JSnippet = JSnippet
  { js_baseSnippets :: T.Text,
    js_methodSnippets :: [T.Text],
    js_bodySnippets :: [JBody],
    js_testSnippets :: [JTest],
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
    then doContext snip
    else
      let code = c_payload (cc_code snip)
          newCode = T.unlines (map rewriteLine (T.lines code))
       in doContext (snip {cc_code = Code newCode})
  where
    doContext snip = do
      context <- getOptionalStringValue (cc_location snip) "context" (cc_args snip)
      contextStart <-
        fromMaybe "" <$> getOptionalStringValue (cc_location snip) "contextStart" (cc_args snip)
      contextEnd <-
        fromMaybe "" <$> getOptionalStringValue (cc_location snip) "contextEnd" (cc_args snip)
      (contextStart2, contextEnd2) <-
        case context of
          Nothing -> pure ("", "")
          Just c ->
            case M.lookup c contextMap of
              Just x -> pure x
              Nothing -> Left ("Unsupported context: " <> c <> ". Known contexts: " <>
                               showText (M.keys contextMap))
      let newCode =
            contextStart <> contextStart2 <> c_payload (cc_code snip) <> contextEnd2 <> contextEnd
      pure (snip { cc_code = Code newCode })
    contextMap =
      M.fromList [("main", ("public static void main(String[] args) {\n", "\n}"))]
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

-- STEP 2&3

annotateAppend :: [CodeSnippet] -> Fail [(CodeSnippet, Bool)] -- bool flag for append
annotateAppend [] = pure []
annotateAppend (x : xs) = do
  append <- getArg "append" x False
  standalone <- getArg "standalone" x True
  xs' <- annotateAppend xs
  case (standalone, xs') of
    (False, ((y, _) : rest)) -> pure $ (x, append) : (y, True) : rest
    (True, rest) -> pure $ (x, append) : rest
    (False, []) ->
      Left
        ( unLocation (cc_location x)
            <> ": standalone:false for last snippet of a group"
        )
  where
    getArg k x def = fromMaybe def <$> getOptionalBoolValue (cc_location x) k (cc_args x)

data PreMergedSnippet = PreMergedSnippet
  { pms_baseSnippets :: [CodeSnippet],
    pms_methodSnippets :: [[CodeSnippet]],
    pms_bodySnippets :: [(Maybe BodyName, [CodeSnippet])],
    pms_testSnippets :: [(Maybe TestName, [CodeSnippet])]
  }
  deriving (Eq, Show)

data SnippetKind
  = SnippetKindRegular
  | SnippetKindBody (Maybe T.Text)
  | SnippetKindMethod
  | SnippetKindTest (Maybe T.Text)
  deriving (Eq, Show)

emptyPreMergedSnippet :: PreMergedSnippet
emptyPreMergedSnippet = PreMergedSnippet [] [] [] []

appendAtLast :: [[a]] -> a -> [[a]]
appendAtLast ll x = modifyLast ll (\l -> l ++ [x]) [[x]]

modifyLast :: [a] -> (a -> a) -> [a] -> [a]
modifyLast l f def =
  case reverse l of
    [] -> def
    y : ys -> reverse (f y : ys)

addToPreMergedSnippet :: SnippetKind -> CodeSnippet -> PreMergedSnippet -> Bool -> PreMergedSnippet
addToPreMergedSnippet k snip pms append =
  case k of
    SnippetKindRegular -> pms { pms_baseSnippets = pms_baseSnippets pms ++ [snip] }
    SnippetKindBody mName ->
      pms { pms_bodySnippets = doAppendNamed (pms_bodySnippets pms) (fmap BodyName mName, snip) }
    SnippetKindMethod -> pms { pms_methodSnippets = doAppend (pms_methodSnippets pms) snip }
    SnippetKindTest mName ->
      pms { pms_testSnippets = doAppendNamed (pms_testSnippets pms) (fmap TestName mName, snip) }
  where
    doAppendNamed :: Eq n => [(Maybe n, [a])] -> (Maybe n, a) -> [(Maybe n, [a])]
    -- unnamed: append to last or create new (depending on value of append)
    doAppendNamed ll (Nothing, x) = doAppendNamed' ll (Nothing, x)
    -- named: append to one with same name (if exists, independent from value of append)
    -- otherwise proceed as in unnamed case
    doAppendNamed ll (Just name, x) =
      case L.span (\(mName, _) -> mName /= Just name) ll of
        (_, []) -> doAppendNamed' ll (Just name, x) -- nothing found
        (prefix, (sameName, xs) : suffix) ->
          prefix ++ ((sameName, xs ++ [x]) : suffix)
    doAppend :: [[a]] -> a -> [[a]]
    doAppend ll x =
      if append then appendAtLast ll x else ll ++ [[x]]
    doAppendNamed' :: [(Maybe n, [a])] -> (Maybe n, a) -> [(Maybe n, [a])]
    doAppendNamed' ll (name, x) =
      if append
        then modifyLast ll (\(n, l) -> (n, l ++ [x])) [(name, [x])]
        else ll ++ [(name, [x])]

data BoolOrStringArg =
  BoolArg Bool
  | StringArg T.Text

snippetKind :: CodeSnippet -> Fail SnippetKind
snippetKind cs = do
  m <- get "method"
  b <- getBoolOrString "body"
  t <- getBoolOrString "test"
  case (m, b, t) of
    (True, BoolArg False, BoolArg False) -> pure SnippetKindMethod
    (False, BoolArg True, BoolArg False) -> pure (SnippetKindBody Nothing)
    (False, StringArg t, BoolArg False) -> pure (SnippetKindBody (Just t))
    (False, BoolArg False, BoolArg True) -> pure (SnippetKindTest Nothing)
    (False, BoolArg False, StringArg t) -> pure (SnippetKindTest (Just t))
    (False, BoolArg False, BoolArg False) -> pure SnippetKindRegular
    _ -> Left "Can have only one of the arguments method, body, and test"
  where
    loc = cc_location cs
    get k = fromMaybe False <$> getOptionalBoolValue loc k (cc_args cs)
    getBoolOrString k =
      case getOptionalBoolValue loc k (cc_args cs) of
        Right (Just b) -> Right (BoolArg b)
        Right Nothing -> Right (BoolArg False)
        Left _ ->
          case getOptionalStringValue loc k (cc_args cs) of
            Right (Just s) -> Right (StringArg s)
            Right Nothing -> Right (BoolArg False)
            Left _ -> Left ("Value for " <> k <> " must either be a bool or a string")

snippetsToText :: [CodeSnippet] -> T.Text
snippetsToText = mkCode javaLangConfig

classFromPreMergeSnippet :: PreMergedSnippet -> Fail (Maybe ClassName)
classFromPreMergeSnippet pms = do
  loop (pms_baseSnippets pms) Nothing
  where
    loop [] acc = pure acc
    loop (codeSnippet:rest) acc = do
      let args = cc_args codeSnippet
          loc = cc_location codeSnippet
      ms <- getOptionalStringValue loc "class" args
      case (ms, acc) of
        (Just t, Just clsName) -> do
          if clsName == ClassName t
            then loop rest acc
            else Left (unLocation loc <> ": conflicting class name for snippet")
        (Just t, Nothing) -> loop rest (Just (ClassName t))
        (Nothing, _) -> loop rest acc

merge :: [CodeSnippet] -> Fail [MergedSnippet]
merge snips = do
  annotatedSnips <- annotateAppend snips
  pms <- loop Nothing annotatedSnips
  mapM toMergedSnippet pms
  where
    toMergedSnippet :: PreMergedSnippet -> Fail MergedSnippet
    toMergedSnippet pms = do
      clear <-
        case pms_baseSnippets pms of
          (x : _) ->
            fromMaybe False <$> getOptionalBoolValue (cc_location x) "clear" (cc_args x)
          [] -> pure False
      cls <- classFromPreMergeSnippet pms
      pure $
        MergedSnippet
          { ms_clear = clear,
            ms_baseSnippets = snippetsToText (pms_baseSnippets pms),
            ms_methodSnippets = map snippetsToText (pms_methodSnippets pms),
            ms_bodySnippets =
              map (\(name, cs) -> JBody name (snippetsToText cs)) (pms_bodySnippets pms),
            ms_testSnippets =
              map (\(name, cs) -> JTest name (snippetsToText cs)) (pms_testSnippets pms),
            ms_locations = map cc_location (pms_baseSnippets pms),
            ms_class = cls
          }
    loop :: Maybe PreMergedSnippet -> [(CodeSnippet, Bool)] -> Fail [PreMergedSnippet]
    loop Nothing [] = pure [emptyPreMergedSnippet]
    loop (Just ms) [] = pure [ms]
    loop Nothing ((snip, _append) : rest) = do
      k <- snippetKind snip
      let newCtx = addToPreMergedSnippet k snip emptyPreMergedSnippet False
      loop (Just newCtx) rest
    loop (Just ms) ((snip, append) : rest) = do
      k <- snippetKind snip
      case append of
        True ->
          let newCtx = addToPreMergedSnippet k snip ms True
          in loop (Just newCtx) rest
        False ->
          case k of
            SnippetKindRegular | not (null (pms_baseSnippets ms)) -> do
              let newCtx = addToPreMergedSnippet k snip emptyPreMergedSnippet False
              restResult <- loop (Just newCtx) rest
              pure (ms : restResult)
            _ -> do
              let newCtx = addToPreMergedSnippet k snip ms False
              loop (Just newCtx) rest

-- STEP 4

parseJava :: [Location] -> T.Text -> Fail (Maybe CompilationUnit)
parseJava locs code = do
  res <- parseJavaOrMembers locs code
  case res of
    Left cu -> pure (Just cu)
    Right _ -> pure Nothing

parseJavaOrMembers :: [Location] -> T.Text -> Fail (Either CompilationUnit [MemberDecl])
parseJavaOrMembers locs code =
  case parse compilationUnit of
    Left errCu ->
      case parse memberDecls of
        Left errMem ->
          let err = if isToplevel then errCu else errMem
              msg = "Error parsing java code at " <> formatLocations locs <> ": " <> showText err
                    <> "\n\nCode:\n\n~~~\n" <> code <> "\n~~~"
          in debugM (T.unpack msg) >> Left msg
        Right x -> return (Right x)
    Right x -> return (Left x)
  where
    parse p = parserWithMode ParseShallow p "<input>" (T.unpack code)
    isToplevel =
      flip any (T.lines code) $ \line ->
        any
          (\p -> p `T.isPrefixOf` line || ("public " <> p) `T.isPrefixOf` line)
          ["class", "enum", "record"]

type P = Parsec.Parsec [JavaLexer.L JavaLexer.Token] ParserState

memberDecls :: P [MemberDecl]
memberDecls = do
  l <- list d
  Parsec.eof
  pure l
  where
    d = do
      loc <- getLocation
      ms <- list modifier
      dec <- memberDecl
      pure (dec loc ms)

-- Returns Nothing if either the code could not be parsed as a compilation unit
-- or if there is no package declared.
getPackageName :: [Location] -> T.Text -> Fail (Maybe PackageName)
getPackageName locs code = do
  mCu <- parseJava locs code
  case mCu of
    Nothing -> pure Nothing
    Just (CompilationUnit mPkgDecl _imports _types) ->
      case mPkgDecl of
        Nothing -> pure Nothing
        Just (PackageDecl name) -> pure (Just (PackageName (nameToText name)))

formatLocations :: [Location] -> T.Text
formatLocations locs =
  T.concat (L.intersperse ", " (map unLocation locs))

-- Example: ["a", "b", "c", "b"] ~~> ["a", "b_v01", "c", "b_v02"]
addVersionsIfNecessary :: [T.Text] -> [T.Text]
addVersionsIfNecessary l =
  let counts :: M.Map T.Text Int
      counts = foldr (\x m -> snd $ M.insertLookupWithKey incOld x 1 m) M.empty l
   in loop counts l M.empty []
  where
    loop _ [] _ acc = reverse acc
    loop counts (x : xs) versionMap acc =
      case M.lookup x counts of
        Just i | i > 1 ->
          case M.insertLookupWithKey incOld x 2 versionMap of
            (Just thisVersion, newVersionMap) ->
              loop counts xs newVersionMap (appendVersion thisVersion x : acc)
            (Nothing, newVersionMap) ->
              loop counts xs newVersionMap (appendVersion 1 x : acc)
        _ ->
          loop counts xs versionMap (x : acc)
    appendVersion i x =
      x <> (if i < 10 then "_v0" else "_v") <> showText i
    incOld _key _new _old = _old + 1

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
      mPkgName <- getPackageName (ms_locations x) (ms_baseSnippets x)
      let pkgName = fromMaybe PackageDefault mPkgName
          group = SnippetGroup pkgName (groupIdFromPkgName pkgName) [x] (ms_class x)
      loop (Just group) xs
    loop (Just group) [] = pure [group]
    loop (Just group) (x : xs) = do
      mPkgName <- do
        mp <- getPackageName (ms_locations x) (ms_baseSnippets x)
        case mp of
          Just _ -> pure mp
          _ ->
            case ms_clear x of
              True -> pure (Just PackageDefault)
              False -> pure Nothing
      case mPkgName of
        Just pkgName -> do
          -- start new group
          let newGroup = SnippetGroup pkgName (groupIdFromPkgName pkgName) [x] (ms_class x)
          rest <- loop (Just newGroup) xs
          pure (group : rest)
        Nothing ->
          loop (Just (group {sg_snippets = sg_snippets group ++ [x]})) xs

-- STEP 5
concatCode :: T.Text -> T.Text -> T.Text
concatCode c1 c2
  | T.strip c1 == "" = c2
  | T.strip c2 == "" = c1
  | otherwise = c1 <> sep <> c2
  where
    sep
      | newlineCount == 0 = "\n\n"
      | newlineCount == 1 = "\n"
      | otherwise = ""
    newlineCount =
      countNewlines (T.takeWhileEnd isSpace c1)
        + countNewlines (T.takeWhile isSpace c2)
    countNewlines = T.count "\n"

data Decl = Decl
  { d_id :: T.Text,
    d_start :: JLocation, -- location of first token
    d_end :: JLocation,   -- location of last token
    d_sub :: [Decl],
    d_public :: Bool
  }
  deriving (Show, Eq)

isPublic :: [Modifier] -> Bool
isPublic mods = Public `elem` mods

compilationUnitToDecls :: CompilationUnit -> [Decl]
compilationUnitToDecls (CompilationUnit _ _ typeDecls) =
  mapMaybe typeDeclToDecl typeDecls
  where
    typeDeclToDecl :: TypeDecl -> Maybe Decl
    typeDeclToDecl (ClassTypeDecl cls) =
      Just $
        case cls of
          ClassDecl (locStart, locEnd) mods ident _ _ _ (ClassBody decls) ->
            Decl (identToText ident) locStart locEnd (mapMaybe declToDecl decls) (isPublic mods)
          RecordDecl (locStart, locEnd) mods ident _ _ _ (ClassBody decls) ->
            Decl (identToText ident) locStart locEnd (mapMaybe declToDecl decls) (isPublic mods)
          EnumDecl (locStart, locEnd) mods ident _ (EnumBody _ decls) ->
            Decl (identToText ident) locStart locEnd (mapMaybe declToDecl decls) (isPublic mods)
    typeDeclToDecl (InterfaceTypeDecl ifaceDecl) =
      Just $
      case ifaceDecl of
        InterfaceDecl (locStart, locEnd) _ mods ident _ _ _ _ ->
            Decl (identToText ident) locStart locEnd [] (isPublic mods)
    declToDecl (JavaSyntax.MemberDecl memDecl) = memberDeclToDecl memDecl
    declToDecl (JavaSyntax.InitDecl _ _) = Nothing

memberDeclToDecl :: MemberDecl -> Maybe Decl
memberDeclToDecl memDecl =
  case memDecl of
    FieldDecl (startLoc, endLoc) mods _ varDecls ->
      let id = T.concat (map (\(VarDecl x _) -> idOfVarDeclId x) varDecls)
       in Just $ Decl id startLoc endLoc [] (isPublic mods)
    MethodDecl (startLoc, endLoc) mods _ _ ident _ _ _ _ ->
      Just $ Decl (identToText ident) startLoc endLoc [] (isPublic mods)
    ConstructorDecl (startLoc, endLoc) mods _ ident _ _ _ ->
      Just $ Decl ("$init_" <> identToText ident) startLoc endLoc [] (isPublic mods)
    MemberClassDecl _ -> Nothing
    MemberInterfaceDecl _ -> Nothing
  where
    idOfVarDeclId (VarId i) = identToText i
    idOfVarDeclId (VarDeclArray i) = idOfVarDeclId i <> "[]"

locationToIndex :: T.Text -> JLocation -> Int
locationToIndex t loc =
  if JavaSyntax.isEof loc
    then T.length t
    else
      let lineIdx = loc_line loc
          colIdx = loc_column loc
          idx = eatLines t lineIdx
       in idx + colIdx - 1
  where
    -- Returns the number of character until line i (index starts at 1)
    eatLines :: T.Text -> Int -> Int
    eatLines t i =
      if i <= 1
        then 0
        else
          let (first, rest) = T.span (/= '\n') t
              n = T.length first
           in case T.uncons rest of
                Nothing -> n
                Just ('\n', rest1) -> n + 1 + eatLines rest1 (i - 1)
                rest -> error ("unexpected result from uncons: " ++ show rest)

getCode :: T.Text -> Decl -> T.Text
getCode t d =
  let start = locationToIndex t (d_start d)
      end = locationToIndex t (d_end d) + 1
   in subText start t end

data CodePatch =
  CodePatchReplace Int T.Text Int
  | CodePatchInsert Int T.Text
  | CodePatchAppend T.Text
  deriving (Show, Eq)

-- Locations in patches are always wrt origCode.
-- The location of a Insert is never inside a Replace.
-- The locations of a replace do never overlap.
patchCode :: T.Text -> [CodePatch] -> T.Text
patchCode origCode patches =
  let sortedPatches = L.sortOn getStartIdx patches
  in foldr applyPatch origCode sortedPatches
  where
    getStartIdx (CodePatchReplace i _ _) = i
    getStartIdx (CodePatchInsert i _) = i
    getStartIdx (CodePatchAppend _) = maxBound
    applyPatch p t =
      case p of
        CodePatchReplace start repl end ->
          insertText start (deleteText start t (end + 1)) repl
        CodePatchInsert i toInsert ->
          insertText i t toInsert
        CodePatchAppend toAppend ->
          t <> toAppend

data AddLocation
  = AddAtEnd
  | AddHere JLocation

mergeWithPrev ::
  MergedSnippet -> [(T.Text, Decl)] -> MergedSnippet -> [Decl] -> AddLocation -> MergedSnippet
mergeWithPrev prevSnip prevDecls thisSnip thisDecls addLoc =
  let patches = map mkPatches thisDecls
  in thisSnip { ms_baseSnippets = patchCode prevCode patches }
  where
    prevCode = ms_baseSnippets prevSnip
    mkPatches :: Decl -> CodePatch
    mkPatches d =
      let thisCode = "\n" <> getCode (ms_baseSnippets thisSnip) d <> "\n"
      in case L.lookup (d_id d) prevDecls of
           Nothing ->
             case addLoc of
               AddAtEnd -> CodePatchAppend thisCode
               AddHere loc -> CodePatchInsert (locationToIndex prevCode loc) thisCode
           Just prevD ->
              CodePatchReplace (locationToIndex prevCode (d_start prevD)) thisCode
                (locationToIndex prevCode (d_end prevD))

{-
  * CASE toplevel.
    The new content of the snippet is the content of the preceding snippet such that
     - declarations that already exist in the preceding snippet are replaced,
     - declarations that do not existing in the preceding snippet are appended.
-}
mergeCu :: MergedSnippet -> CompilationUnit -> MergedSnippet -> CompilationUnit -> MergedSnippet
mergeCu prevSnip prevCu snip cu =
  let prevDecls = map (\d -> (d_id d, d)) (compilationUnitToDecls prevCu)
      thisDecls = compilationUnitToDecls cu
   in mergeWithPrev prevSnip prevDecls snip thisDecls AddAtEnd

data MainClassMode = MainClassForFilename | MainClassForPatching
{-
  If not given via groupClass or in snip:
  * First public class
  * First non-public class
-}
findMainClass ::
    MainClassMode -> Maybe ClassName -> MergedSnippet -> CompilationUnit -> Fail (Maybe Decl)
findMainClass mode groupClass snip cu =
  let decls = compilationUnitToDecls cu
      public = L.find d_public decls
   in case (mode, public) of
        (MainClassForFilename, Just d) -> pure (Just d)
        _ ->
          case mainClassName of
              Just name ->
                case L.find (\d -> d_id d == name) decls of
                  Just d -> pure (Just d)
                  Nothing -> Left ("Unknown main class specified at locations " <>
                                    showText (map unLocation (ms_locations snip)))
              Nothing ->
                case public of
                    Just d -> pure (Just d)
                    Nothing ->
                      case decls of
                        [] -> pure Nothing
                        (x : _) -> pure (Just x)
  where
    mainClassName =
      fmap unClassName $
      case mode of
        MainClassForFilename -> groupClass
        MainClassForPatching -> (ms_class snip `mplus` groupClass)

{-
  * CASE nested, i.e. the code contains only methods. The plugin tries to find methods with
    the same name in the toplevel declarations of the preceding snippet.
    - If it finds such a method, the new content of the snippet is the content of the preceding
      snippet with the method being updated.
    - If no such method exists, the new content of the snippet is the content of the preceding
      snippet with the method being appended to the content of the main class of the preceding
      snippet.
-}
mergeMembers ::
  Maybe ClassName -> MergedSnippet -> CompilationUnit -> MergedSnippet -> [MemberDecl]
  -> Fail MergedSnippet
mergeMembers mClassName prevSnip prevCu snip methods = do
  mainClass <- findMainClass MainClassForPatching mClassName snip prevCu
  case mainClass of
    Nothing ->
      Left
      ( formatLocations (ms_locations snip)
        <> ": no main class found in preceding snippet but this snippets seems to define methods"
      )
    Just mc -> do
      let prevDecls =
            concatMap (\d -> map (\sub -> (d_id sub, sub)) (d_sub d))
                (compilationUnitToDecls prevCu)
          thisDecls = map memberDeclToDecl methods
          thisDeclsGood = catMaybes thisDecls
      if length thisDecls /= length thisDeclsGood
        then
          Left
            ( formatLocations (ms_locations snip)
                <> ": cannot handle inner classes or interfaces"
            )
        else do
          debugM ("Merging " ++ show (length methods) ++
                  " methods into previous compilation unit ...")
          pure $ mergeWithPrev prevSnip prevDecls snip thisDeclsGood (AddHere (d_end mc))

updateSnippetContent :: Maybe ClassName -> MergedSnippet -> MergedSnippet -> Fail MergedSnippet
updateSnippetContent mClassName prevSnip snip = do
  mPrevCu <- parseJava (ms_locations prevSnip) (ms_baseSnippets prevSnip)
  mThisParsed <- parseJavaOrMembers (ms_locations snip) (ms_baseSnippets snip)
  case (mPrevCu, mThisParsed) of
    (Just prevCu, Left thisCu) ->
      pure $ mergeCu prevSnip prevCu snip thisCu
    (Just prevCu, Right thisMethods) ->
      mergeMembers mClassName prevSnip prevCu snip thisMethods
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
      newX <- updateSnippetContent (sg_mainClass sg) prev x
      rest <- loop newX xs
      pure (newX : rest)

-- STEP 6
getMainClassName :: Maybe ClassName -> MergedSnippet -> [Location] -> T.Text -> Fail (Maybe ClassName)
getMainClassName groupClass snip locs code = do
  mCu <- parseJava locs code
  case mCu of
    Nothing -> pure Nothing
    Just cu -> do
      mClass <- findMainClass MainClassForFilename groupClass snip cu
      pure (fmap (ClassName . d_id) mClass)

_classDeclIdent :: ClassDecl -> Ident
_classDeclIdent d =
  case d of
    ClassDecl _ _ className _ _ _ _ -> className
    RecordDecl _ _ recName _ _ _ _ -> recName
    EnumDecl _ _ enumName _ _ -> enumName

identToText :: Ident -> T.Text
identToText (Ident s) = T.pack s

nameToText :: Name -> T.Text
nameToText (Name idents) =
  T.intercalate "." (map identToText idents)

snippetGroupToJSnippets :: CodeFilePath -> SnippetGroup -> Fail [JSnippet]
snippetGroupToJSnippets key group =
  mapM toJSnippet (zip [1 ..] (sg_snippets group))
  where
    toJSnippet (v, ms) = do
      let mClsNameFromKey =
            case key of
              CodeFilePathDefault -> Nothing
              CodeFilePathCustom fp ->
                if ".java" `L.isSuffixOf` fp
                  then Just (ClassName $ T.pack $ takeBaseName fp)
                  else Nothing
      mClsName <- getMainClassName (sg_mainClass group) ms (ms_locations ms) (ms_baseSnippets ms)
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
            js_mainClass = fromMaybe (ClassName "Main") (mClsNameFromKey `mplus` mClsName),
            js_key = key
          }

-- Orchestration

checkForNoTabs :: CodeSnippet -> Action ()
checkForNoTabs cs = do
  let t = c_payload (cc_code cs)
  when (T.any (== '\t') t) $
    fail (T.unpack (unLocation (cc_location cs) <> ": found TAB in snippet"))

processSnippets :: CodeFilePath -> [CodeSnippet] -> Action [JSnippet]
processSnippets key snippets = do
  debug ("snippets: " ++ show snippets)
  forM_ snippets checkForNoTabs
  -- Step 1
  snippets <- failInM $ mapM rewrite snippets
  debug ("After step 1, snippets: " ++ show snippets)
  -- Step 2&3
  merged <- failInM (merge snippets)
  debug ("After step 2&3, merged: " ++ show merged)
  -- Step 4
  groups <- failInM (groupSnippets merged)
  debug ("After step 4, groups: " ++ show groups)
  -- Step 5
  newGroups <- failInM (mapM updateGroup groups)
  debug ("After step 5, newGroups: " ++ show newGroups)
  -- Step 6
  result <- failInM (concatMapM (snippetGroupToJSnippets key) newGroups)
  debug ("After step 6, result: " ++ show result)
  pure result

jsnippetCode :: [CodeSnippet] -> JSnippet -> [CodeSnippet] -> T.Text
jsnippetCode start snip end =
  let startCode = snippetsToText start
      endCode = snippetsToText end
      snipCode = js_baseSnippets snip
   in startCode `concatCode` snipCode `concatCode` endCode
        `concatCode` container (js_bodySnippets snip) (js_methodSnippets snip) (js_testSnippets snip)
  where
    container bodies methods tests =
      let methodsForBodies = flip map (zip [1..] bodies) $ \(i, JBody mBodyName code) ->
            let bodyName = fromMaybe (showText i) (fmap unBodyName mBodyName)
            in "public static void __body_" <> bodyName <> "() throws Exception {\n"
                 <> code
                 <> "\n}"
          methodsForTests = flip map (zip [1..] tests) $ \(i, JTest mTestName code) ->
            let testName = fromMaybe ("__test_" <> showText i) (fmap unTestName mTestName)
            in "@Test public void " <> testName <> "() throws Exception {\n"
               <> code
               <> "\n}"
          allMethods = methods ++ methodsForBodies ++ methodsForTests
       in if null allMethods
            then ""
            else
              let code = T.concat (L.intersperse "\n\n" allMethods)
               in "class __CodeContainer {\n" <> code <> "\n}"

outputJSnippet ::
  FilePath ->
  T.Text ->
  [CodeSnippet] ->
  JSnippet ->
  [CodeSnippet] ->
  [JSnippet] ->
  S.Set CodeFilePath ->
  Action ()
outputJSnippet outDir header start snip end allSnippets allKeys = do
  let code = jsnippetCode start snip end
      key =
        case js_key snip of
          CodeFilePathDefault -> Nothing
          CodeFilePathCustom fp ->
            if S.size allKeys < 2
              then Nothing
              else Just (dropExtensions fp)
      groupId =
        if moreThanOne js_key js_group
          then Just (T.unpack (unGroupId $ js_group snip))
          else Nothing
      version =
        if moreThanOne (\x -> (js_key x, js_group x)) js_version
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
        outDir </> "code" </>
        (L.intercalate "/" (catMaybes [key, groupId, version, pkgName]))
      file = dir </> T.unpack (unClassName (js_mainClass snip)) <.> "java"
  note ("Generating " ++ file)
  myWriteFile file (header <> code)
  where
    moreThanOne groupingExtract extract =
      let loop [] = False
          loop (x : xs) =
            (groupingExtract x == groupingExtract snip &&
             extract x /= extract snip)
            || loop xs
       in loop allSnippets

processCodeMap ::
  BuildConfig ->
  BuildArgs ->
  LangConfig ->
  T.Text ->
  CodeMap ->
  [CodeSnippet] ->
  Action ()
processCodeMap _buildCfg buildArgs _langCfg header cm allCodeSnippets = do
  let outDir = pluginDir (PluginName (lc_name javaLangConfig))
  jSnippetsWithStartEnd <- flip concatMapM (M.toList cm) $ \(k, ccf) -> do
    l <- processSnippets k (ccf_here ccf)
    pure $ map (\j -> (ccf_atStart ccf, j, ccf_atEnd ccf)) l
  let allJSnippets = map (\(_, j, _) -> j) jSnippetsWithStartEnd
  forM_ jSnippetsWithStartEnd $ \(start, snip, end) ->
    outputJSnippet
      outDir
      header
      start
      snip
      end
      allJSnippets
      (M.keysSet cm)
  let fullCode = header <> snippetsToText allCodeSnippets
      file = outDir </> takeBaseName (ba_inputFile buildArgs) <.> "java"
  myWriteFile file fullCode

javaLangConfig :: LangConfig
javaLangConfig =
  (mkLangConfig "java" ".java" "// " Nothing processCodeMap)
    { lc_extraArgs = ["method", "body", "test", "append", "standalone", "clear", "rewrite",
                      "context", "contextStart", "contextEnd", "class"]
    }
