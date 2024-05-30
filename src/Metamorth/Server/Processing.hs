{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Server.Processing
  ( makeProcessFunc ) where

import Control.Applicative

import Data.Functor.Identity

import Data.Maybe

import Data.Map.Strict qualified as M

import Metamorth.Server.ForOutput.Types

import Language.Haskell.TH

import Metamorth.Server.Helpers

import THLego.Helpers

import Data.Text qualified as T

import Data.Text.Lazy          qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE

{-
      data ConvertMessage = ConvertMessage
        { cmInputText  :: T.Text
        , cmInputOrth  :: $inOrthType
        , cmOutputOrth :: $outOrthType
        } deriving (Show, Eq)

      -- | Version that allows querying of the converter in general.
      data QueryMessage
        = ConvertQuery ConvertMessage
        | InformationQuery
        deriving (Show, Eq)

-}

makeProcessFunc :: Q [Dec]
makeProcessFunc = do
  inMapName    <- maybeLookupValue  "inputOrthNameMap"  inputErr
  outMapName   <- maybeLookupValue "outputOrthNameMap" outputErr
  inOrthType   <- maybeLookupType  "InOrth"  inOrthErr
  outOrthType  <- maybeLookupType "OutOrth" outOrthErr

  languageDet <- maybeLookupValue "languageDetails" "Can't find \"languageDetails\"."

  convertMsgType <- maybeLookupType  "ConvertMessage" "Can't find \"ConvertMessage\" type."
  convertMsgCons <- maybeLookupValue "ConvertMessage" "Can't find \"ConvertMessage\" constructor."
  queryMsgType   <- maybeLookupType  "QueryMessage" "Can't find \"QueryMessage\" type."
  -- queryMsgCons   <- maybeLookupValue "QueryMessage" "Can't find \"QueryMessage\" constructor."

  infoQueryCons  <- maybeLookupValue "InformationQuery" "Can't find \"InformationQuery\" constructor."
  convQueryCons  <- maybeLookupValue "ConvertQuery" "Can't find \"ConvertQuery\" constructor."

  mainFunction <- maybeLookupValue "convertOrthographyBS" "Can't find the main converter function."

  textSelector   <- maybeLookupValue "cmInputText"  "Can't find the input text selector."
  inputSelector  <- maybeLookupValue "cmInputOrth"  "Can't find the input orth selector."
  outputSelector <- maybeLookupValue "cmOutputOrth" "Can't find the output orth selector."

  let conMsgTQ  = pure $ ConT convertMsgType
      -- infoQryEQ = pure $ ConT infoQueryType
      infoQryPQ = pure $ ConP infoQueryCons [] []
      -- convQryNQ = pure convQueryCons
      
{-
      data ConvertMessage = ConvertMessage
        { cmInputText  :: T.Text
        , cmInputOrth  :: $(pure $ ConT  inOrthType)
        , cmOutputOrth :: $(pure $ ConT outOrthType)
        } deriving (Show, Eq)
-}

  cmName <- newName "cm"
  
  [d|
      revInMap :: M.Map $(pure $ ConT inOrthType) String
      revInMap = invertOrthMap $(pure $ VarE inMapName)

      revOutMap :: M.Map $(pure $ ConT outOrthType) String
      revOutMap = invertOrthMap $(pure $ VarE inMapName)

      mainLangName :: T.Text
      mainLangName = fromMaybe "Unknown Language" (fst $(pure $ VarE languageDet))

      mainOrthDescs :: M.Map T.Text T.Text
      mainOrthDescs = M.mapKeys T.pack $ M.map T.pack $ snd $(pure $ VarE languageDet)

      mainOrthData :: [OrthData]
      mainOrthData = runIdentity $ forM (M.toAscList mainOrthDescs) $ \(orthName, orthDesc) -> do
        -- Try looking up the name to find the constructor...
        let inConstM  = M.lookup orthName $(pure $ VarE  inMapName)
            outConstM = M.lookup orthName $(pure $ VarE outMapName)
            -- Use the constructor to lookup the possible names...
            inNamesM  = inConstM  >>= \cstr -> M.lookup cstr revInMap
            outNamesM = outConstM >>= \cstr -> M.lookup cstr revOutMap
            -- See which one (if any) worked.
            orthNoms  = inNamesM <|> outNamesM
        return $ OrthData orthName orthDesc (fromMaybe [] orthNoms)
        

      processInput :: $conMsgTQ -> ResponseValue
      processInput $infoQryPQ = InfoResponse (ConverterInfo mainLangName [])
      processInput $(pure (ConP convQueryCons [] [VarP cmName])) = 
        case ($(pure $ multiAppE 
                  (VarE mainFunction) 
                  [ AppE (VarE  inputSelector) (VarE cmName)
                  , AppE (VarE outputSelector) (VarE cmName)
                  , AppE (VarE   textSelector) (VarE cmName)
                  ])) of
          (Left err) -> QueryResponse $ FailedConvert  (T.pack err)
          (Right bs) -> QueryResponse $ SuccessConvert (TL.toStrict $ TLE.decodeUtf8 bs)
   |]

{-
data ConverterInfo = ConverterInfo
  { languageName :: T.Text
  , orthData     :: [OrthData]
  } deriving (Show, Eq)


data ResponseValue
  = QueryResponse ResponseMessage
  | InfoResponse ConverterInfo
  deriving (Show, Eq)
   
-}

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


