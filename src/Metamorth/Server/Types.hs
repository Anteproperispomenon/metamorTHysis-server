{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Metamorth.Server.Types
  ( makeJSONTypes
  ) where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Encoding (string, text)

import Data.Function (on)
import Data.List (nubBy)
import Data.Tuple (swap)

import Data.Map.Strict qualified as M
-- import Data.Set        qualified as S

import Metamorth.Server.Helpers

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Text qualified as T

makeJSONTypes :: Q [Dec]
makeJSONTypes = do
  inMapName    <- maybeLookupValue  "inputOrthNameMap"  inputErr
  outMapName   <- maybeLookupValue "outputOrthNameMap" outputErr
  inOrthType   <- maybeLookupType  "InOrth"  inOrthErr
  outOrthType  <- maybeLookupType "OutOrth" outOrthErr

  [d| 
      inMapText  :: M.Map T.Text $(pure $ ConT inOrthType)
      inMapText = M.mapKeys T.pack $(pure $ VarE inMapName)

      outMapText :: M.Map T.Text $(pure $ ConT outOrthType)
      outMapText = M.mapKeys T.pack $(pure $ VarE outMapName)

      revInMap  :: M.Map $(pure $ ConT  inOrthType) T.Text
      revInMap  = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs  inMapText

      revOutMap :: M.Map $(pure $ ConT outOrthType) T.Text
      revOutMap = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs outMapText
      

      -- Orphan instances, but there's not really a good
      -- place to put these elsewhere.
      instance FromJSON $(pure $ ConT inOrthType) where
        parseJSON = withText "InOrth" $ \txt -> case (M.lookup (T.toLower txt) inMapText) of
          Nothing -> fail $ "Couldn't parse \"" ++ T.unpack txt ++ "\" as an input orthography."
          (Just iorth) -> return iorth
      
      instance FromJSON $(pure $ ConT outOrthType) where
        parseJSON = withText "OutOrth" $ \txt -> case (M.lookup (T.toLower txt) outMapText) of
          Nothing -> fail $ "Couldn't parse \"" ++ T.unpack txt ++ "\" as an output orthography."
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
          Nothing    -> string $ drop 2 $ show val -- temp?

      instance ToJSON $(pure $ ConT outOrthType) where
        toJSON val = case (M.lookup val revOutMap) of
          (Just txt) -> String txt
          Nothing    -> String $ T.pack $ drop 3 $ show val -- temp?
        toEncoding val = case (M.lookup val revOutMap) of
          (Just txt) -> text txt
          Nothing    -> string $ drop 3 $ show val -- temp?

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
      
      instance ToJSON ConvertMessage where
        toJSON (ConvertMessage txt iOrth oOrth) = object
          [ "text"   .= txt
          , "input"  .= iOrth
          , "output" .= oOrth
          ]
        toEncoding (ConvertMessage txt iOrth oOrth) = pairs
          (  "text"   .= txt
          <> "input"  .= iOrth
          <> "output" .= oOrth
          )

      -- | Version that allows querying of the converter in general.
      data QueryMessage
        = ConvertQuery ConvertMessage
        | InformationQuery
        deriving (Show, Eq)
      
      instance FromJSON QueryMessage where
        parseJSON v = (ConvertQuery <$> parseJSON @ConvertMessage v) <|>
          withText "QueryMessage" (\txt -> case T.toLower txt of
              "info"          -> InformationQuery
              "information"   -> InformationQuery
              "details"       -> InformationQuery
              "help"          -> InformationQuery
              "list"          -> InformationQuery
              "orth"          -> InformationQuery
              "orthographies" -> InformationQuery
              _ -> fail "Unrecognised message."
            ) v
      
      instance ToJSON QueryMessage where
        toJSON (ConvertQuery x) = toJSON x
        toJSON InformationQuery = String "info"

        toEncoding (ConvertQuery x) = toEncoding x
        toEncoding InformationQuery = text "info"
      
      -- Now, the converted message:
      data ResponseMessage
        = SuccessConvert T.Text
        | PartialConvert T.Text T.Text
        | FailedConvert  T.Text
        deriving (Show, Eq)
      
      instance ToJSON ResponseMessage where
        toJSON (SuccessConvert txt) = object
          [ "text"  .= txt ]
        toJSON (PartialConvert txt err) = object
          [ "text"  .= txt, "error" .= err ]
        toJSON (FailedConvert err) = object
          [ "error" .= err ]
        toEncoding (SuccessConvert txt) = pairs
          ( "text"  .= txt )
        toEncoding (PartialConvert txt err) = pairs
          ( "text"  .= txt <> "error" .= err )
        toEncoding (FailedConvert err) = pairs
          ( "error" .= err )
      
      instance FromJSON ResponseMessage where
        parseJSON = withObject "ResponseMessage" $ \v -> mkVal =<< (,)
            <$> v .:? "text"
            <*> v .:? "error"
          where
            mkVal (Just txt, Nothing ) = return $ SuccessConvert txt
            mkVal (Just txt, Just err) = return $ PartialConvert txt err
            mkVal (Nothing , Just err) = return $ FailedConvert  err
            mkVal _ = fail "Couldn't parse response object."

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


