module Metamorth.Server.Helpers
  ( maybeLookupValue
  , maybeLookupType
  , invertOrthMap
  , lookupValueExpDef
  , lookupValueExpLift
  ) where

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

maybeLookupValue :: String -> String -> Q Name
maybeLookupValue valName err = do
  mNom <- lookupValueName valName
  case mNom of
    Nothing -> do
      reportError err
      return $ mkName valName -- standin.
    (Just x) -> return x

maybeLookupType :: String -> String -> Q Name
maybeLookupType typName err = do
  mNom <- lookupTypeName typName
  case mNom of
    Nothing -> do
      reportError err
      return $ mkName typName -- standin.
    (Just x) -> return x

lookupValueExpDef :: String -> Exp -> Q Exp
lookupValueExpDef strName defVal = do
  mNom <- lookupValueName strName
  case mNom of
    (Just nom) -> do
      nomInfo <- reify nom
      case nomInfo of
        (ClassOpI nom2 _ _) -> return $ VarE nom2
        (DataConI nom2 _ _) -> return $ ConE nom2
        (PatSynI  nom2 _)   -> return $ ConE nom2 -- I think this works for pattern synonyms.
        (VarI     nom2 _ _) -> return $ VarE nom2
        _ -> return defVal
    Nothing -> return defVal

lookupValueExpLift :: (Lift a) => String -> a -> Q Exp
lookupValueExpLift strName val = lift val >>= lookupValueExpDef strName

-- | Invert a map. From metamorTHysis-cli
invertOrthMap :: (Ord inOrth, Ord txt) => M.Map txt inOrth -> M.Map inOrth (S.Set txt)
invertOrthMap = M.foldlWithKey (\mp' k val -> insertWithElse S.insert S.singleton val k mp') M.empty
-- invertOrthMap inMap = M.foldlWithKey (\mp' k val -> insertWithElse S.insert S.singleton val k mp') M.empty inMap

-- | Like `insertWith`, but the type of the value to be
--   inserted doesn't have to be the same as value already
--   inserted. Also taken from metamorTHysis-cli.
insertWithElse :: (Ord k) => (w -> v -> v) -> (w -> v) -> k -> w -> M.Map k v -> M.Map k v
insertWithElse op f k val
  = M.alter (\case {Nothing -> Just $ f val ; (Just y) -> Just $ op val y}) k

