module Metamorth.Server.ForOutput.Types
  ( ResponseValue(..)
  , ResponseMessage(..)
  , ConverterInfo(..)
  , OrthData(..)
  ) where

-- Types that don't depend on the values of the
-- generated code.

import Control.Applicative

import Data.Aeson
import Data.Aeson.Encoding (text)

import Data.Text qualified as T

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
      
data OrthData = OrthData
  { orthName :: T.Text
  , orthDesc :: T.Text
  , orthArgs :: [T.Text] -- the short names for this orthography
  } deriving (Show, Eq)

instance FromJSON OrthData where
  parseJSON = withObject "OrthData" $ \v -> OrthData
    <$> v .: "name"
    <*> ((v .: "desc") <|> (v .: "description"))
    <*> ((v .: "args") <|> (v .: "short-names"))
      
instance ToJSON OrthData where
  toJSON od = object
    [ "name" .= (orthName od)
    , "desc" .= (orthDesc od)
    , "args" .= (orthArgs od)
    ]
  toEncoding od = pairs
    (  "name" .= (orthName od)
    <> "desc" .= (orthDesc od)
    <> "args" .= (orthArgs od)
    )


data ConverterInfo = ConverterInfo
  { languageName :: T.Text
  , orthData     :: [OrthData]
  } deriving (Show, Eq)

instance FromJSON ConverterInfo where
  parseJSON = withObject "ConverterInfo" $ \v -> ConverterInfo
    <$> ((v .: "language") <|> (v .: "name"))
    <*> (v .: "data")

instance ToJSON ConverterInfo where
  toJSON ci = object
    [ "language" .= (languageName ci)
    , "data"     .= (orthData ci)
    ]
  toEncoding ci = pairs
    (  "language" .= (languageName ci)
    <> "data"     .= (orthData ci)
    )

data ResponseValue
  = QueryResponse ResponseMessage
  | InfoResponse ConverterInfo
  deriving (Show, Eq)
   
instance FromJSON ResponseValue where
  parseJSON v = (QueryResponse <$> parseJSON v) <|> (InfoResponse <$> parseJSON v)

instance ToJSON ResponseValue where
  toJSON (QueryResponse rm) = toJSON rm
  toJSON (InfoResponse  im) = toJSON im
  toEncoding (QueryResponse rm) = toEncoding rm
  toEncoding (InfoResponse  im) = toEncoding im