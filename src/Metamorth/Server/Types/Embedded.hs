{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Server.Types.Embedded
  ( makeJSONTypes
  ) where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Encoding (text)

import Data.Maybe

import Data.Typeable

import Data.Function (on)
import Data.List (nubBy)
import Data.Tuple (swap)

import Data.Map.Strict qualified as M
-- import Data.Set        qualified as S

import Metamorth.Lift.Pat

import Metamorth.Server.Helpers

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Text qualified as T

import LiftType

makeJSONTypes 
  :: forall iorth oorth. (Show iorth, Show oorth, Ord iorth, Ord oorth, Enum iorth, Enum oorth, Lift iorth, Lift oorth, Bounded iorth, Bounded oorth, Typeable iorth, Typeable oorth) 
  => M.Map String (iorth)
  -> M.Map String (oorth, String)
  -> Q [Dec]
makeJSONTypes inputMap' outputMap'  = do
  let inputMap  = inputMap'
      outputMap = M.map fst outputMap'
 
  -- inMapName    <- maybeLookupValue  "inputOrthNameMap"  inputErr
  -- outMapName   <- maybeLookupValue "outputOrthNameMap" outputErr
  -- inOrthType   <- maybeLookupType  "InOrth"  inOrthErr
  -- outOrthType  <- maybeLookupType "OutOrth" outOrthErr
  
  inOrthTypeX  <- liftTypeQ @iorth
  outOrthTypeX <- liftTypeQ @oorth


  let inMapText  = M.mapKeys T.pack  inputMap
      outMapText = M.mapKeys T.pack outputMap
      revInMap   = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs  inMapText
      revOutMap  = M.fromList $ map swap $ nubBy ((==) `on` snd) $ M.assocs outMapText

      inConstructors  = [minBound..maxBound] :: [iorth]
      outConstructors = [minBound..maxBound] :: [oorth]

      inOrthType  = pure  inOrthTypeX
      outOrthType = pure outOrthTypeX

      -- The lookup maps.
      inLookups   = mapMaybe (\x -> (x,) <$> (M.lookup x  revInMap))  inConstructors
      outLookups  = mapMaybe (\x -> (x,) <$> (M.lookup x revOutMap)) outConstructors
  
      mkStringJSON :: T.Text -> Exp
      mkStringJSON txt = AppE (ConE 'String) (LitE (StringL $ T.unpack txt))

      mkStringEncd :: T.Text -> Exp
      mkStringEncd txt = AppE (VarE 'text) (LitE (StringL $ T.unpack txt))

  -- Doing the "ToJSON" instances manually...
  inClauses' <- forM inLookups $ \(val, txt) -> do
    mNewPat <- liftPat val
    case mNewPat of
      Nothing -> do 
        reportWarning $ "Can't create a pattern for orthography value \"" ++ show val ++ "\"."
        return Nothing
      (Just newPat) -> do
        return $ Just
          ( Clause [newPat] (NormalB $ mkStringJSON txt) []
          , Clause [newPat] (NormalB $ mkStringEncd txt) []
          )
  
  let inClauses = catMaybes inClauses'
      
  outClauses' <- forM outLookups $ \(val, txt) -> do
    mNewPat <- liftPat val
    case mNewPat of
      Nothing -> do 
        reportWarning $ "Can't create a pattern for orthography value \"" ++ show val ++ "\"."
        return Nothing
      (Just newPat) -> do
        return $ Just
          ( Clause [newPat] (NormalB (mkStringJSON txt)) []
          , Clause [newPat] (NormalB (mkStringEncd txt)) []
          )
  
  let outClauses = catMaybes outClauses'

  -- Separating out the pairs of clauses
  let ( inClauses1,  inClauses2) = unzip  inClauses
      (outClauses1, outClauses2) = unzip outClauses

  -- Finally, actually creating the instance declarations...
  let inOrthInstance = InstanceD Nothing [] (AppT (ConT ''ToJSON) inOrthTypeX)
        [ FunD 'toJSON inClauses1
        , FunD 'toEncoding inClauses2
        ]

  let outOrthInstance = InstanceD Nothing [] (AppT (ConT ''ToJSON) outOrthTypeX)
        [ FunD 'toJSON outClauses1
        , FunD 'toEncoding outClauses2
        ]

  mainDecs <- [d| 
      
      -- Orphan instances, but there's not really a good
      -- place to put these elsewhere.
      instance FromJSON $inOrthType where
        parseJSON = withText "InOrth" $ \txt -> case (M.lookup (T.toLower txt) inMapText) of
          Nothing -> fail $ "Couldn't parse \"" ++ T.unpack txt ++ "\" as an input orthography."
          (Just iorth) -> return iorth
      
      instance FromJSON $outOrthType where
        parseJSON = withText "OutOrth" $ \txt -> case (M.lookup (T.toLower txt) outMapText) of
          Nothing -> fail $ "Couldn't parse \"" ++ T.unpack txt ++ "\" as an output orthography."
          (Just oorth) -> return oorth
        
      -- These next two should have declarations generated using
      -- the map as a base, but we don't have direct access to the
      -- map; we can only use it inside quotations.
      {-
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
      -}

      data ConvertMessage = ConvertMessage
        { cmInputText  :: T.Text
        , cmInputOrth  :: $inOrthType
        , cmOutputOrth :: $outOrthType
        } deriving (Show, Eq)
      
      instance FromJSON ConvertMessage where
        parseJSON = withObject "ConvertMessage" $ \v -> ConvertMessage
          <$> v .: "text"
          <*> ((v .:  "input") <|> (v .:  "inputOrth") <|> (v .:  "inOrth"))
          <*> ((v .: "output") <|> (v .: "outputOrth") <|> (v .: "outOrth"))
      
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
              "info"          -> return InformationQuery
              "information"   -> return InformationQuery
              "details"       -> return InformationQuery
              "help"          -> return InformationQuery
              "list"          -> return InformationQuery
              "orth"          -> return InformationQuery
              "orthographies" -> return InformationQuery
              _ -> fail "Unrecognised message."
            ) v
      
      instance ToJSON QueryMessage where
        toJSON (ConvertQuery x) = toJSON x
        toJSON InformationQuery = String "info"

        toEncoding (ConvertQuery x) = toEncoding x
        toEncoding InformationQuery = text "info"

   |]
   
  return (inOrthInstance:outOrthInstance:mainDecs)


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


