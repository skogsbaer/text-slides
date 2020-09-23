module LatexRules (latexRules) where

import qualified Data.List as L
import qualified Data.Text as T
import Development.Shake
import Logging
import System.FilePath
import Types
import Utils

texBodyExt :: String
texBodyExt = ".texbody"

texPreambleExt :: String
texPreambleExt = ".texpreamble"

preambleCacheExt :: String
preambleCacheExt = ".fmt"

genPdf :: BuildConfig -> FilePath -> Action ()
genPdf cfg pdf = do
  let input = pdf -<.> texBodyExt
  need [input, pdf -<.> preambleCacheExt]
  mySystem
    INFO
    (bc_pdflatex cfg)
    [ "-halt-on-error",
      "-interaction",
      "nonstopmode",
      "-output-directory",
      bc_buildDir cfg,
      input
    ]

-- FIXME: rerun if necessary

splitPreamble :: FilePath -> T.Text -> Maybe (T.Text, T.Text)
splitPreamble outFile src =
  case L.break isEndOfPreamble (T.lines src) of
    ([], _) -> Nothing
    (_, []) -> Nothing
    (prefix, endLine : body) ->
      let preamble = T.unlines prefix <> endLine
          fullBody = "%&" <> jobname <> "\n" <> T.unlines body
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
      note ("Generating " ++ texPreambleExt)
      myWriteFile texPreamble preamble

genFmt :: BuildConfig -> FilePath -> Action ()
genFmt cfg fmt = do
  let texPreamble = fmt -<.> texPreambleExt
  preamble <- myReadFile texPreamble
  if preamble == ""
    then mySystem INFO "touch" [fmt]
    else do
      let jobname = takeBaseName fmt
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

latexRules :: BuildConfig -> BuildArgs -> Rules ()
latexRules cfg _args = do
  (bc_buildDir cfg ++ "//*.pdf") %> genPdf cfg
  (bc_buildDir cfg ++ ("//*") <> texBodyExt) %> genTexBody
  (bc_buildDir cfg ++ ("//*") <> texPreambleExt) %> genTexPreamble
  (bc_buildDir cfg ++ "//*.fmt") %> genFmt cfg
