{-# LANGUAGE MultiWayIf #-}

module Parser where

import Control.Monad
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Void
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Types

data Token
  = Line T.Text
  | Plugin PluginCall
  deriving (Eq, Show)

type Parser = P.Parsec Void T.Text

spaceP :: Parser ()
spaceP =
  L.space
    (void P.spaceChar)
    (fail "no support for line comments")
    (fail "no support for block comments")

lexemeP :: Parser a -> Parser a
lexemeP = L.lexeme spaceP

symbolP :: T.Text -> Parser ()
symbolP = void . L.symbol spaceP

someSymbolP :: Parser T.Text
someSymbolP = do
  first <- P.letterChar
  rest <- P.many (P.satisfy (\c -> isAlphaNum c || c == '_' || c == '-'))
  return $ T.pack (first : rest)

pluginCallPrefixP :: Parser PluginName
pluginCallPrefixP = do
  symbolP "~~~"
  name <- someSymbolP
  return (PluginName name)

getPluginName :: T.Text -> Maybe PluginName
getPluginName line =
  case P.parse pluginCallPrefixP "<input>" line of
    Left _ -> Nothing
    Right x -> Just x

parensP :: Parser a -> Parser a
parensP = P.between (symbolP "(") (symbolP ")")

pluginCallP :: Parser PluginCall
pluginCallP = do
  name <- lexemeP pluginCallPrefixP
  args <- lexemeP $ P.option [] $ parensP (P.sepBy (lexemeP argP) (symbolP ","))
  P.option () $ symbolP "~~~"
  return $
    PluginCall
      { pc_pluginName = name,
        pc_args = M.fromList args,
        pc_body = "",
        pc_sectionName = Nothing,
        pc_location = unknownLocation
      }
  where
    argP = do
      k <- lexemeP someSymbolP
      lexemeP (symbolP ":")
      v <- argValueP
      return (k, v)

stringLiteralP :: Parser T.Text
stringLiteralP =
  T.pack <$> (lexemeP $ P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

intP :: Parser Int
intP = do
  n <- L.signed P.space (lexemeP L.decimal)
  return (fromInteger n)

boolP :: Parser Bool
boolP =
  (symbolP "true" >> return True) P.<|> (symbolP "false" >> return False)

argValueP :: Parser ArgValue
argValueP =
  (ArgString <$> stringLiteralP)
    P.<|> (ArgInt <$> intP)
    P.<|> (ArgBool <$> boolP)

parsePluginCall :: String -> Maybe T.Text -> T.Text -> Fail PluginCall
parsePluginCall location mSectionName line =
  case P.parse (pluginCallP <* P.eof) location line of
    Left err -> Left $ T.pack (P.errorBundlePretty err)
    Right x -> Right (x {pc_location = Location (T.pack location), pc_sectionName = mSectionName})

data ParseContext = ParseContext
  { pc_revTokens :: [Token],
    pc_currentCall :: Maybe (PluginCall, [T.Text]),
    pc_currentSectionName :: Maybe T.Text,
    pc_skipLine :: Bool
    -- the list contains the reversed lines of the plugin's body
  }

emptyParseContext :: ParseContext
emptyParseContext = ParseContext [] Nothing Nothing False

isPluginBodyEnd :: T.Text -> Bool
isPluginBodyEnd t = T.stripEnd t == "~~~"

parseMarkdown :: FilePath -> M.Map PluginName PluginKind -> T.Text -> Fail [Token]
parseMarkdown file plugins input = do
  let lines = T.lines input
      nextLines = map Just (drop 1 lines) ++ [Nothing]
  ctx <- foldM handleLine emptyParseContext (zip3 [1 ..] lines nextLines)
  case pc_currentCall ctx of
    Just (call, _) ->
      Left $
        T.pack file <> ": unterminated call of plugin " <> unPluginName (pc_pluginName call)
    Nothing ->
      Right (reverse (pc_revTokens ctx))
  where
    handleLine ctx (lineNo, line, mNextLine)
      | pc_skipLine ctx = return $ ctx {pc_skipLine = False}
      | otherwise =
        case pc_currentCall ctx of
          Just (call, revCallLines) ->
            if isPluginBodyEnd line
              then
                return $
                  ctx
                    { pc_revTokens =
                        Plugin
                          ( call
                              { pc_body =
                                  T.stripEnd (T.unlines (reverse revCallLines))
                              }
                          ) :
                        pc_revTokens ctx,
                      pc_currentCall = Nothing,
                      pc_skipLine = False
                    }
              else
                return $
                  ctx
                    { pc_currentCall = Just (call, line : revCallLines),
                      pc_skipLine = False
                    }
          Nothing ->
            case getPluginName line of
              Just name
                | Just k <- M.lookup name plugins -> do
                  call <-
                    parsePluginCall (file ++ ":" ++ show lineNo) (pc_currentSectionName ctx) line
                  case k of
                    PluginWithoutBody -> do
                      let skipNext =
                            case mNextLine of
                              Just t -> isPluginBodyEnd t
                              Nothing -> False
                      return $
                        ctx
                          { pc_revTokens = Plugin call : pc_revTokens ctx,
                            pc_skipLine = skipNext
                          }
                    PluginWithBody ->
                      return $ ctx {pc_currentCall = Just (call, []), pc_skipLine = False}
              _ -> do
                let sectionName = parseSectionName line
                return $
                  ctx
                    { pc_revTokens = Line line : pc_revTokens ctx,
                      pc_currentSectionName =
                        case sectionName of
                          Just x -> Just x
                          Nothing -> pc_currentSectionName ctx,
                      pc_skipLine = False
                    }

parseSectionName :: T.Text -> Maybe T.Text
parseSectionName t =
  if "##" `T.isPrefixOf` t
    then
      let title = T.drop 2 t
       in if "#" `T.isPrefixOf` title
            then Nothing
            else
              Just $
                T.strip $
                  removeShrink $
                    T.dropWhileEnd (\c -> c == '#') $ removeShrink title
    else Nothing
  where
    removeShrink :: T.Text -> T.Text
    removeShrink t =
      let t1 = T.stripEnd t
       in case T.stripSuffix "{.shrink}" t1 of
            Just x -> T.stripEnd x
            Nothing -> t1
