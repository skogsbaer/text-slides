module LatexRules (latexRules) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import Logging
import System.Environment
import System.FilePath
import Types
import Utils

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
mkLatexEnv cfg = do
  oldEnv <- M.fromList . map (\(x, y) -> (T.pack x, T.pack y)) <$> liftIO getEnvironment
  let sep = T.singleton searchPathSeparator
      texInputs =
        case M.lookup texInputsKey oldEnv of
          Nothing -> T.pack (bc_buildDir cfg) <> sep <> ""
          Just old -> T.pack (bc_buildDir cfg) <> sep <> old
  return $ M.insert texInputsKey texInputs oldEnv
  where
    texInputsKey = "TEXINPUTS"

genPdf :: BuildConfig -> FilePath -> Action ()
genPdf cfg pdf = do
  need [pdf -<.> texBodyExt, pdf -<.> preambleCacheExt]
  env <- liftIO $ mkLatexEnv cfg
  note ("Generating " ++ pdf)
  runPdfLatex env 0
  where
    runPdfLatex env runNo = do
      mySystem
        INFO
        (bc_pdflatex cfg)
        [ "-halt-on-error",
          "-interaction",
          "nonstopmode",
          "-output-directory",
          bc_buildDir cfg,
          pdf -<.> texBodyExt
        ]
        (Just env)
      -- check if we must re-run latex
      let logFile = pdf -<.> ".log"
      log <- liftIO $ T.readFile logFile -- do not depend on the log file
      let mRerunPhrase = findPhrase log rerunPhrases
      case mRerunPhrase of
        Nothing -> return ()
        Just phrase -> do
          if runNo > 3
            then
              warn $
                "Not re-running latex, already did " ++ show (runNo + 1) ++ " runs but "
                  ++ "still found the following in the log: "
                  ++ T.unpack phrase
            else do
              note $ "Re-running latex, reason: " ++ T.unpack phrase
              runPdfLatex env (runNo + 1)

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

genFmt :: BuildConfig -> FilePath -> Action ()
genFmt cfg fmt = do
  let texPreamble = fmt -<.> texPreambleExt
  preamble <- myReadFile texPreamble
  if preamble == ""
    then mySystem INFO "touch" [fmt] Nothing
    else do
      let jobname = takeBaseName fmt
      env <- liftIO $ mkLatexEnv cfg
      note ("Generating " ++ fmt)
      mySystem
        INFO
        (bc_pdflatex cfg)
        [ "-ini",
          "-interaction",
          "nonstopmode",
          "-output-directory",
          bc_buildDir cfg,
          "-jobname=" ++ jobname,
          "&pdflatex",
          "mylatexformat.ltx",
          texPreamble
        ]
        (Just env)

latexRules :: BuildConfig -> BuildArgs -> Rules ()
latexRules cfg _args = do
  (bc_buildDir cfg ++ "//*.pdf") %> genPdf cfg
  (bc_buildDir cfg ++ ("//*") <> texBodyExt) %> genTexBody
  (bc_buildDir cfg ++ ("//*") <> texPreambleExt) %> genTexPreamble
  (bc_buildDir cfg ++ "//*.fmt") %> genFmt cfg
