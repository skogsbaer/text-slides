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
  rest <- P.many (P.satisfy (\c -> isAlphaNum c || c == '_'))
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
  args <- P.option [] $ parensP (P.sepBy (lexemeP argP) (symbolP ","))
  return $
    PluginCall
      { pc_pluginName = name,
        pc_args = M.fromList args,
        pc_body = "",
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

parsePluginCall :: String -> T.Text -> Fail PluginCall
parsePluginCall location line =
  case P.parse (pluginCallP <* P.eof) location line of
    Left err -> Left $ T.pack (P.errorBundlePretty err)
    Right x -> Right (x {pc_location = Location (T.pack location)})

data ParseContext = ParseContext
  { pc_revTokens :: [Token],
    pc_currentCall :: Maybe (PluginCall, [T.Text])
    -- the list contains the reversed lines of the plugin's body
  }

emptyParseContext :: ParseContext
emptyParseContext = ParseContext [] Nothing

parseMarkdown :: FilePath -> M.Map PluginName PluginKind -> T.Text -> Fail [Token]
parseMarkdown file plugins input = do
  ctx <- foldM handleLine emptyParseContext (zip [1 ..] (T.lines input))
  case pc_currentCall ctx of
    Just (call, _) ->
      Left $
        T.pack file <> ": unterminated call of plugin " <> unPluginName (pc_pluginName call)
    Nothing ->
      Right (reverse (pc_revTokens ctx))
  where
    handleLine ctx (lineNo, line) =
      let rstripped = T.stripEnd line
       in case pc_currentCall ctx of
            Just (call, revCallLines) ->
              if rstripped == "~~~"
                then
                  return $
                    ParseContext
                      { pc_revTokens =
                          Plugin
                            ( call
                                { pc_body =
                                    T.stripEnd (T.unlines (reverse revCallLines))
                                }
                            ) :
                          pc_revTokens ctx,
                        pc_currentCall = Nothing
                      }
                else return $ ctx {pc_currentCall = Just (call, line : revCallLines)}
            Nothing ->
              case getPluginName line of
                Just name
                  | Just k <- M.lookup name plugins -> do
                    call <- parsePluginCall (file ++ ":" ++ show lineNo) line
                    case k of
                      PluginWithoutBody ->
                        return $
                          ctx {pc_revTokens = Plugin call : pc_revTokens ctx}
                      PluginWithBody ->
                        return $ ctx {pc_currentCall = Just (call, [])}
                _ -> return $ ctx {pc_revTokens = Line line : pc_revTokens ctx}
