module LatexRules (latexRules) where

import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Shake
import Logging
import Safe
import qualified System.Directory as Dir
import System.Environment
import System.FilePath
import Types
import Utils
import RuleUtils

texBodyExt :: String
texBodyExt = ".texbody"

texPreambleExt :: String
texPreambleExt = ".texpreamble"

preambleCacheExt :: String
preambleCacheExt = ".fmt"

rerunPhrases :: [T.Text]
rerunPhrases =
  [ "Table widths have changed. Rerun LaTeX.",
    "Rerun to get outlines right",
    "Rerun to get cross-references right"
  ]

findPhrase :: T.Text -> [T.Text] -> Maybe T.Text
findPhrase _ [] = Nothing
findPhrase t (x : xs) =
  if x `T.isInfixOf` t then Just x else findPhrase t xs

mkLatexEnv :: BuildConfig -> IO Env
mkLatexEnv _cfg = do
  oldEnv <- M.fromList . map (bimap T.pack T.pack) <$> liftIO getEnvironment
  let sep = T.singleton searchPathSeparator
      texInputs =
        case M.lookup texInputsKey oldEnv of
          Nothing -> T.pack buildDir <> sep <> ""
          Just old -> T.pack buildDir <> sep <> old
  return $ M.insert texInputsKey texInputs oldEnv
  where
    texInputsKey = "TEXINPUTS"

data Frame = Frame
  { f_title :: T.Text,
    f_slideNo :: Int,
    f_lineStart :: Int,
    f_lineEnd :: Int
  }

parseFrameFromTex :: T.Text -> [Frame]
parseFrameFromTex text =
  let frames = reverse $ loop [] Nothing (zip (T.lines text) [1 ..])
   in zipWith (\f slideNo -> f {f_slideNo = slideNo}) frames [2 ..]
  where
    loop acc _ [] = acc
    loop acc ctx ((line, no) : rest)
      | "\\begin{frame}" `T.isPrefixOf` line =
        loop acc (Just (Frame (extractTitle line) 0 no no)) rest
      | "\\end{frame}" `T.isPrefixOf` line,
        Just f <- ctx =
        loop (f {f_lineEnd = no} : acc) Nothing rest
      | otherwise = loop acc ctx rest
    extractTitle t =
      let t1 = T.drop (T.length "\\begin{frame}{") t
          t2 = if "fragile" `T.isPrefixOf` t1 then T.drop (T.length "fragile]{") t1 else t1
       in T.dropEnd 1 t2

findFrame :: [Frame] -> Int -> Maybe Frame
findFrame [] _ = Nothing
findFrame (f : fs) lineNo =
  if lineNo >= f_lineStart f && lineNo <= f_lineEnd f
    then Just f
    else findFrame fs lineNo

checkForOverfullSlides :: FilePath -> Action ()
checkForOverfullSlides pdf = do
  logLines <- (map T.stripEnd . T.lines) <$> liftIO (readFileWithUnknownEncoding log)
  let overfullVBoxes = mapMaybe findOverfullVBox logLines
  texSrc <- liftIO $ readFileWithUnknownEncoding tex
  let frames = parseFrameFromTex texSrc
  forM_ overfullVBoxes (warnOverfullVBox frames)
  where
    log = pdf -<.> ".log"
    tex = pdf -<.> ".texbody"
    warnOverfullVBox :: [Frame] -> (T.Text, Int) -> Action ()
    warnOverfullVBox frames (errMsg, lineInTex) = do
      let raw = log ++ ": " ++ T.unpack errMsg
      case findFrame frames lineInTex of
        Nothing -> warn raw
        Just frame ->
          warn
            ( "Slide " ++ show (f_slideNo frame) ++ " " ++ show (f_title frame)
                ++ " is too high. ("
                ++ raw
                ++ ")"
            )
    findOverfullVBox :: T.Text -> Maybe (T.Text, Int)
    findOverfullVBox line =
      if "Overfull \\vbox" `T.isPrefixOf` line
        then case readMay (T.unpack $ L.last (T.words line)) of
          Just texLineNo -> Just (line, texLineNo)
          Nothing -> Nothing
        else Nothing

readFileWithUnknownEncoding :: FilePath -> IO T.Text
readFileWithUnknownEncoding fp = do
  bs <- BS.readFile fp -- no dependency
  case T.decodeUtf8' bs of
    Left _exc -> return $ T.pack $ BSC.unpack bs -- assume it's iso-8859-1
    Right t -> return t

genPdf :: FilePath -> Action ()
genPdf pdf = do
  cfg <- getBuildConfig
  need [pdf -<.> texBodyExt, pdf -<.> preambleCacheExt]
  deps <- readDeps pdf
  need deps
  info ("Deps for " ++ pdf ++ ": " ++ show deps)
  env <- liftIO $ mkLatexEnv cfg
  note ("Generating " ++ pdf)
  runPdfLatex cfg env 0
  checkForOverfullSlides pdf
  where
    runPdfLatex cfg env runNo = do
      let logFile = pdf -<.> ".log"
          navFile = pdf -<.> ".nav"
      navExists <- liftIO $ Dir.doesFileExist navFile
      navBefore <- if navExists then liftIO $ BS.readFile navFile else return BS.empty
      mySystem
        INFO
        DontPrintStdout
        (bc_pdflatex cfg)
        [ "-halt-on-error",
          "-interaction",
          "nonstopmode",
          "-output-directory",
          buildDir,
          pdf -<.> texBodyExt
        ]
        (Just env)
      navAfter <- liftIO $ BS.readFile navFile
      -- check if we must re-run latex
      log <- liftIO $ readFileWithUnknownEncoding logFile -- do not depend on the log file
      let mRerunPhrase = findPhrase log rerunPhrases
      needRerun <-
        case (mRerunPhrase, navAfter == navBefore) of
          (Nothing, True) -> return False
          (Just phrase, _) -> do
            if runNo > maxReruns
              then do
                warn $
                  "Not re-running latex, already did " ++ show (runNo + 1) ++ " runs but "
                    ++ "still found the following in the log: "
                    ++ T.unpack phrase
                return False
              else do
                note $ "Re-running latex, reason: " ++ T.unpack phrase
                return True
          (Nothing, False) -> do
            if runNo > maxReruns
              then do
                warn $
                  "Not re-running latex, already did " ++ show (runNo + 1) ++ " runs but "
                    ++ " frame numbers are still wrong"
                return False
              else do
                note "Re-running latex because frame numbers changed"
                return True
      when needRerun $ runPdfLatex cfg env (runNo + 1)
    maxReruns = 3

splitPreamble :: FilePath -> T.Text -> Maybe (T.Text, T.Text)
splitPreamble outFile src =
  case L.break isEndOfPreamble (T.lines src) of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (prefix, endLine : body) ->
      let preamble = T.unlines prefix <> endLine
          fullBody = "%&" <> jobname <> "\n\\endofdump\n" <> T.unlines body
       in Just (preamble, fullBody)
  where
    jobname = T.pack $ dropExtension outFile
    isEndOfPreamble t =
      "\\endofdump" `T.isPrefixOf` t

genTexBody :: FilePath -> Action ()
genTexBody texBody = do
  let tex = texBody -<.> ".tex"
  texSrc <- myReadFile tex
  note ("Generating " ++ texBody)
  case splitPreamble texBody texSrc of
    Nothing -> myWriteFile texBody texSrc
    Just (_, body) -> myWriteFile texBody body

genTexPreamble :: FilePath -> Action ()
genTexPreamble texPreamble = do
  let tex = texPreamble -<.> ".tex"
  texSrc <- myReadFile tex
  case splitPreamble texPreamble texSrc of
    Nothing -> myWriteFile texPreamble ""
    Just (preamble, _) -> do
      note ("Generating " ++ texPreamble)
      myWriteFile texPreamble preamble

genFmt :: FilePath -> Action ()
genFmt fmt = do
  cfg <- getBuildConfig
  let texPreamble = fmt -<.> texPreambleExt
  preamble <- myReadFile texPreamble
  if preamble == ""
    then mySystem INFO DontPrintStdout "touch" [fmt] Nothing
    else do
      let jobname = takeBaseName fmt
      env <- liftIO $ mkLatexEnv cfg
      note ("Generating " ++ fmt)
      mySystem
        INFO
        DontPrintStdout
        (bc_pdflatex cfg)
        [ "-ini",
          "-interaction",
          "nonstopmode",
          "-output-directory",
          buildDir,
          "-jobname=" ++ jobname,
          "&pdflatex",
          "mylatexformat.ltx",
          texPreamble
        ]
        (Just env)

latexRules :: BuildArgs -> Rules ()
latexRules args = do
  (mainOutputFile args "pdf") %> genPdf
  (d ++ ("/*") <> texBodyExt) %> genTexBody
  (d ++ ("/*") <> texPreambleExt) %> genTexPreamble
  (d ++ "/*.fmt") %> genFmt
  where
    d = sbc_buildDir staticBuildConfig
