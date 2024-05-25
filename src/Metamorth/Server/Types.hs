{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Server.Types
  (

  ) where

import Control.Applicative

import Data.Aeson
import Data.Aeson.Encoding (string, text)

import Data.Function (on)
import Data.List (sort, nubBy)
import Data.Maybe
import Data.Tuple (swap)
import Data.Typeable

import Data.Map.Strict qualified as M
import Data.Set        qualified as S

import Metamorth.Server.Helpers

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Text qualified as T

makeJSONTypes' :: forall io oo. (Ord io, Enum io, Typeable io, Ord oo, Enum oo, Typeable oo) => M.Map String io -> M.Map String oo -> Q [Dec]
makeJSONTypes' inMapStr outMapStr = do
  -- These will be joined later on.
  -- inMapName    <- maybeLookupValue  "inputOrthNameMap"  inputErr
  -- outMapName   <- maybeLookupValue "outputOrthNameMap" outputErr
  -- inOrthType   <- maybeLookupType  "InOrth"  inOrthErr
  -- outOrthType  <- maybeLookupType "OutOrth" outOrthErr

  let inMapText  = M.mapKeys T.pack inMapStr
      outMapText = M.mapKeys T.pack outMapStr
      revInMap   = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs  inMapText
      revOutMap  = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs outMapText

      inList  = [minBound..maxBound] :: [io]
      outList = [minBound..maxBound] :: [oo]

  [d| 
      -- inMapText  :: M.Map T.Text $(pure $ ConT inOrthType)
      -- inMapText = M.mapKeys T.pack $(pure $ VarE inMapName)

      -- outMapText :: M.Map T.Text $(pure $ ConT outOrthType)
      -- outMapText = M.mapKeys T.pack $(pure $ VarE outMapName)

      -- revInMap  :: M.Map $(pure $ ConT  inOrthType) T.Text
      -- revInMap  = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs  inMapText

      -- revOutMap :: M.Map $(pure $ ConT outOrthType) T.Text
      -- revOutMap = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs outMapText

      -- Orphan instances, but there's not really a good
      -- place to put these elsewhere.
      instance FromJSON $(pure $ ConT inOrthType) where
        parseJSON = withText "InOrth" $ \txt -> case (M.lookup (T.toLower txt) inMapText) of
          Nothing -> fail $ "Couldn't parse \"" ++ (T.unpack txt) ++ "\" as an input orthography."
          (Just iorth) -> return iorth
      
      instance FromJSON $(pure $ ConT outOrthType) where
        parseJSON = withText "OutOrth" $ \txt -> case (M.lookup (T.toLower txt) outMapText) of
          Nothing -> fail $ "Couldn't parse \"" ++ (T.unpack txt) ++ "\" as an output orthography."
          (Just oorth) -> return oorth
        
      -- These next two should have declarations generated using
      -- the map as a base, but we don't have direct access to the
      -- map; we can only use it inside quotations.
      instance ToJSON $(pure $ ConT inOrthType) where
        toJSON val = case (M.lookup val revInMap) of
          (Just txt) -> String txt
          Nothing    -> String $ T.pack $ drop 2 $ show val -- temp?
        toEncoding val = case (M.lookup val revInMap) of
          (Just txt) -> text txt
          Nothing    -> string drop 2 $ show val -- temp?

      instance ToJSON $(pure $ ConT outOrthType) where
        toJSON val = case (M.lookup val revOutMap) of
          (Just txt) -> String txt
          Nothing    -> String $ T.pack $ drop 3 $ show val -- temp?
        toEncoding val = case (M.lookup val revOutMap) of
          (Just txt) -> text txt
          Nothing    -> string drop 3 $ show val -- temp?

      -- parseInType :: T.Text -> Maybe $(pure $ ConT inOrthType)
      -- parseInType txt = M.lookup (T.toLower txt) $(pure $ VarE inMapName)
    
      -- parseOutType :: T.Text -> Maybe $(pure $ ConT outOrthType)
      -- parseOutType txt = M.lookup (T.toLower txt) $(pure $ VarE outMapName)

      data ConvertMessage = ConvertMessage
        { cmInputText  :: T.Text
        , cmInputOrth  :: $(pure $ ConT  inOrthType)
        , cmOutputOrth :: $(pure $ ConT outOrthType)
        } deriving (Show, Eq)
      
      instance FromJSON ConvertMessage where
        parseJSON = withObject "ConvertMessage" $ \v -> ConvertMessage
          <$> v .: "text"
          <*> (parseInType  <$> ((v .:  "input") <|> (v .:  "inputOrth") <|> (v .:  "inOrth")))
          <*> (parseOutType <$> ((v .: "output") <|> (v .: "outputOrth") <|> (v .: "outOrth")))
      
      -- instance ToJSON ConvertMessage where
      --  toJSON (ConvertMessage txt iOrth oOrth)
        

  |]



-- Taken from metamorTHysis-cli
inputErr :: String
inputErr = unlines
  [ "Can't find the input orthography map; make sure"
  , "you add \"inputOrthNameMap\" to the export list"
  , "of the orthography file."
  ]

outputErr :: String
outputErr = unlines
  [ "Can't find the output orthography map; make sure"
  , "you add \"outputOrthNameMap\" to the export list"
  , "of the orthography file."
  ]

inOrthErr :: String
inOrthErr = unlines
  [ "Can't find the input Orthography type; make sure"
  , "you add \"InOrth(..)\" to the export list of the"
  , "orthography file."
  ]

outOrthErr :: String
outOrthErr = unlines
  [ "Can't find the output Orthography type; make sure"
  , "you add \"OutOrth(..)\" to the export list of the"
  , "orthography file."
  ]


