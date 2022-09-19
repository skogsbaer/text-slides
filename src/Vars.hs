module Vars
  ( readVarsFile,
    readVarsFromBs,
    expandVars,
    varCount,
    VarMap(..)
  )
where

import qualified Data.Aeson as J
import qualified Data.HashMap.Strict as Hm
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Development.Shake
import Data.Yaml as Y
import Data.Char
import Utils (myReadFileBs)
import qualified Data.ByteString as BS

newtype VarMap = VarMap (M.Map T.Text T.Text)
  deriving (Eq, Show)

varCount :: VarMap -> Int
varCount (VarMap m) = M.size m

instance J.FromJSON VarMap where
    parseJSON (J.Object v) = do
      kvs <- mapM parseKv (Hm.toList v)
      pure $ VarMap (M.fromList kvs)
      where
        parseKv (k, (J.String t)) = pure (k, t)
        parseKv (k, (J.Number n)) = pure (k, T.pack (show n))
        parseKv (k, (J.Bool b)) = pure (k, T.pack (show b))
        parseKv (k, J.Null) = pure (k, "")
        parseKv (_, invalid) = fail ("Expected a primitive, got " ++ show invalid)
    parseJSON invalid = fail ("Expected a mapping, got " ++ show invalid)

readVarsFile :: FilePath -> Action VarMap
readVarsFile fp = do
    bs <- myReadFileBs fp
    readVarsFromBs fp bs

readVarsFromBs :: MonadFail m => FilePath -> BS.ByteString -> m VarMap
readVarsFromBs fp bs =
    case Y.decodeEither' bs of
        Right vm -> pure vm
        Left err -> fail ("Error parsing file " <> fp <> ": " <> show err)

expandVars :: VarMap -> T.Text -> T.Text
expandVars (VarMap m) t = T.concat (loop t)
  where
    start = "{{"
    end = "}}"
    loop t =
      case nextVar t of
        Nothing -> [t]
        Just (prefix, var, suffix) ->
          case M.lookup var m of
              Nothing -> [prefix, start, var, end] ++ loop suffix
              Just x -> [prefix, x] ++ loop suffix
    nextVar t = do
      (prefix, rest) <- splitOn start t
      let (var, suffix) = T.span validVarChar rest
      if end `T.isPrefixOf` suffix
        then pure (prefix, var, T.drop (T.length end) suffix)
        else do
            (prefix2, var2, suffix2) <- nextVar suffix
            pure (prefix <> start <> var <> prefix2, var2, suffix2)
    splitOn x t =
      let (prefix, suffix) = T.breakOn x t
      in if T.null suffix then Nothing else Just (prefix, T.drop (T.length x) suffix)
    validVarChar = isAlphaNum

