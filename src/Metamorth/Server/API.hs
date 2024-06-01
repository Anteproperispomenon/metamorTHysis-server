{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Server.API
  ( makeServantTypes

  ) where

import Data.Aeson

import Data.Text qualified as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Servant

-- We can import this module, since the types
-- in it are known before compile time.
import Metamorth.Server.ForOutput.Types 
import Metamorth.Server.Helpers

makeServantTypes :: Q [Dec]
makeServantTypes = do
  
  qryMsgType <- maybeLookupType "QueryMessage"   "Couldn't find \"QueryMessage\" type."
  conMsgType <- maybeLookupType "ConvertMessage" "Couldn't find \"ConvertMessage\" type."

  convInfo  <- maybeLookupValue "mainInfo"     "Couldn't find \"mainInfo\" value."
  procQuery <- maybeLookupValue "processInput" "Couldn't find \"processInput\" function."

  qryCon1 <- maybeLookupValue "ConvertQuery"     "Couldn't find \"ConvertQuery\" constructor."
  qryCon2 <- maybeLookupValue "InformationQuery" "Couldn't find \"InformationQuery\" constructor."
   
  -- simple name
  -- nm <- newName "nm"

  let qryMsgQ = (pure :: a -> Q a) $ ConT qryMsgType
      conMsgQ = (pure :: a -> Q a) $ ConT conMsgType
      convInfoQ = (pure :: a -> Q a) $ VarE convInfo
      procFuncQ = (pure :: a -> Q a) $ VarE procQuery
      qryCon1EQ = (pure :: a -> Q a) $ ConE qryCon1
      -- qryCon2EQ = (pure :: a -> Q a) $ ConE qryCon2
      -- qryCon1PQ = (pure :: a -> Q a) $ ConP qryCon1 [] [VarP nm]
      -- qryCon2PQ = (pure :: a -> Q a) $ ConP qryCon2 [] []

  mainDecs <- 
    [d| -- Just requesting info, without a request body.
        type InfoAPI = "info" :> Get '[JSON] ConverterInfo 

        -- Main Queries
        type ConvertAPI = "convert" :> ReqBody '[JSON] $conMsgQ :> Post '[JSON] ResponseMessage
        type QueryAPI   = "query"   :> ReqBody '[JSON] $qryMsgQ :> Post '[JSON] ResponseValue

        -- Fallback Query
        type FallbackAPI = "" :> Get '[PlainText, JSON] T.Text

        type MainAPI = InfoAPI :<|> ConvertAPI :<|> QueryAPI :<|> FallbackAPI

        -- Defining the server...
        mainServer :: Server MainAPI
        mainServer = giveInfo :<|> convertData :<|> giveResponse :<|> giveHelp
          where
            giveInfo :: Handler ConverterInfo
            giveInfo = return $convInfoQ

            convertData :: $conMsgQ -> Handler ResponseMessage
            convertData conMsg = case ($procFuncQ ($qryCon1EQ conMsg)) of
              (QueryResponse rspMsg) -> return rspMsg
              (InfoResponse  _     ) -> return $ FailedConvert "This error shouldn't be able to be reached."
            
            giveResponse :: $qryMsgQ -> Handler ResponseValue
            giveResponse qryMsg = return ($procFuncQ qryMsg)

            giveHelp :: Handler T.Text
            giveHelp = return textExplanation
        
        serveQueries :: Application
        serveQueries = serve (Proxy @MainAPI) mainServer

    |]

  -- In case we want to add extra stuff from a separate quote.
  return mainDecs


textExplanation :: T.Text
textExplanation = T.unlines
  [ "Will have an actual explanation of how to use this"
  , "server later on. For now, try going to" 
  , "\"localhost:<portnumber>/info\"."
  ]




