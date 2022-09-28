{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Plugins
  ( htf_thisModulesTests,
  )
where

import Cmdline
import qualified Data.List as L
import qualified Data.Set as S
import Driver
import System.Directory
import System.FilePath
import Test.Framework
import Types
import Utils
import Temp
import Data.Maybe
import System.Process
import System.Exit
import Control.Monad
import Language.Java.Parser
import Language.Java.Syntax hiding (Decl, Location)

runTextSlides :: FilePath -> (FilePath -> IO ()) -> IO ()
runTextSlides mdFile checkFun = withSysTempDir deleteIfNoException "text-slides-test" $ \dir -> do
  copyFile mdFile (dir </> takeFileName mdFile)
  let opts =
        emptyCmdlineOpts
          { co_inputFile = Just (takeFileName mdFile),
            co_outputs = S.fromList [OutputHtml]
          }
  withCurrentDirectory dir (mainWithOpts opts)
  checkFun dir

checkCmd :: String -> IO ()
checkCmd cmd = do
  ecode <- system cmd
  case ecode of
    ExitSuccess -> return ()
    _ -> fail ("FAILED: " ++ cmd)

parseJava :: FilePath -> IO CompilationUnit
parseJava fp = do
  code <- readFile fp
  case parserWithState pState compilationUnit fp code of
    Left err -> fail (fp ++ ": parse error: " ++ show err)
    Right cu -> pure cu
  where
    pState =
      ParserState ParseFull False -- no locations

compareJava :: FilePath -> FilePath -> IO ()
compareJava reference generated = do
  refCu <- parseJava reference
  genCu <- parseJava generated
  unless (refCu == genCu) $ do
    _ <- system ("diff -u " ++ reference ++ " " ++ generated)
    fail ("Generated file " ++ generated ++ " differs from reference file " ++ reference)

test_java :: IO ()
test_java = runTextSlides "test/data/test_java.md" $ \outDir -> do
  let pluginDir = outDir </> "build/plugins/java/"
      referenceDir = "test/data/test_java_reference"
  genFiles <- myListDirectoryRecursive outDir (\p -> ".java" `L.isSuffixOf` p)
  let expectedFiles = javaFiles pluginDir
      stripOutDir l =
        flip map l $ \s -> fromMaybe s (L.stripPrefix (outDir ++ "/") s)
      notGenerated = stripOutDir expectedFiles L.\\ stripOutDir genFiles
      extra = stripOutDir genFiles L.\\ stripOutDir expectedFiles
  assertListsEqualAsSetsVerbose
    ("notGenerated=" ++ show notGenerated ++ ", extra=" ++ show extra)
    expectedFiles genFiles
  forM_ expectedFiles $ \f ->
      unless (isAllCodeFile f) $ do
        putStrLn ("Compiling " ++ f)
        checkCmd ("javac -cp " ++ jarFile ++ " " ++ f)
  let referenceFiles = javaFiles referenceDir
  forM_ (zip referenceFiles expectedFiles) $ \(ref, gen) ->
    unless (isAllCodeFile ref) $ compareJava ref gen
  where
    jarFile = "test/data/junit-platform-console-standalone-1.8.1.jar"
    isAllCodeFile f = "test_java.java" `L.isSuffixOf` f
    javaFiles dir =
      let outAll = dir </> "test_java.java"
          codeDir = dir </> "code"
          outDef1 = codeDir </> "default_pkg/v01/C1.java"
          outDef2 = codeDir </> "default_pkg/v02/C1.java"
          outFoo1 = codeDir </> "foo_v01/foo/Main.java"
          outFoo2 = codeDir </> "foo_v02/foo/C1.java"
          outFoo3 = codeDir </> "foo_v03/v01/foo/C2.java"
          outFoo4 = codeDir </> "foo_v03/v02/foo/C2.java"
          outFoo5 = codeDir </> "foo_v03/v03/foo/C2.java"
          outFoo6 = codeDir </> "foo_v03/v04/foo/C2.java"
          outFoo7 = codeDir </> "foo_v03/v05/foo/C2.java"
          outFoo8 = codeDir </> "foo_v03/v06/foo/C2.java"
          outAlt1 = codeDir </> "alternative/default_pkg_v01/v01/C1.java"
          outAlt2 = codeDir </> "alternative/default_pkg_v01/v02/C1.java"
          outAlt3 = codeDir </> "alternative/default_pkg_v02/C3.java"
          outAlt4 = codeDir </> "alternative2/C1.java"
          outAppend = codeDir </> "Append/C1.java"
       in [ outAll,
            outDef1,
            outDef2,
            outFoo1,
            outFoo2,
            outFoo3,
            outFoo4,
            outFoo5,
            outFoo6,
            outFoo7,
            outFoo8,
            outAlt1,
            outAlt2,
            outAlt3,
            outAlt4,
            outAppend
          ]
