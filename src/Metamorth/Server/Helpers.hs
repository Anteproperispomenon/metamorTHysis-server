module Metamorth.Server.Helpers
  ( maybeLookupValue
  , maybeLookupType
  ) where

import Language.Haskell.TH

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