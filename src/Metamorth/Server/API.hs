{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Server.API
  ( makeServantTypes
  , makeServantTypesNoHtml
  ) where

import Data.Aeson

import Data.Text qualified as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Servant
import Servant.Server.StaticFiles

-- We can import this module, since the types
-- in it are known before compile time.
import Metamorth.Server.ForOutput.Types 
import Metamorth.Server.Helpers

import Network.Wai.Handler.Warp (run, setTimeout, defaultSettings, setPort, runSettings)

makeServantTypes :: Q [Dec]
makeServantTypes = makeServantTypes' True

makeServantTypesNoHtml :: Q [Dec]
makeServantTypesNoHtml = makeServantTypes' False

makeServantTypes' :: Bool -> Q [Dec]
makeServantTypes' useRaw = do
  
  qryMsgType <- maybeLookupType "QueryMessage"   "Couldn't find \"QueryMessage\" type."
  conMsgType <- maybeLookupType "ConvertMessage" "Couldn't find \"ConvertMessage\" type."

  convInfo  <- maybeLookupValue "mainInfo"     "Couldn't find \"mainInfo\" value."
  procQuery <- maybeLookupValue "processInput" "Couldn't find \"processInput\" function."

  qryCon1 <- maybeLookupValue "ConvertQuery"     "Couldn't find \"ConvertQuery\" constructor."
  -- qryCon2 <- maybeLookupValue "InformationQuery" "Couldn't find \"InformationQuery\" constructor."
   
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
        type FallbackAPI = Get '[PlainText, JSON] T.Text

        type MainAPI = InfoAPI :<|> ConvertAPI :<|> QueryAPI :<|> FallbackAPI

        type StaticAPI = "interactive" :> Raw
        type NewAPI    = MainAPI :<|> StaticAPI

        -- Defining the server...
        mainServer :: Server MainAPI
        mainServer = giveInfo :<|> convertData :<|> giveResponse :<|> giveHelp
          where
            giveInfo :: forall m. (Monad m) => m ConverterInfo
            giveInfo = return $convInfoQ

            convertData :: forall m. (Monad m) => $conMsgQ -> m ResponseMessage
            convertData conMsg = case ($procFuncQ ($qryCon1EQ conMsg)) of
              (QueryResponse rspMsg) -> return rspMsg
              (InfoResponse  _     ) -> return $ FailedConvert "This error shouldn't be able to be reached."
            
            giveResponse :: forall m. (Monad m) => $qryMsgQ -> m ResponseValue
            giveResponse qryMsg = return ($procFuncQ qryMsg)

            giveHelp :: forall m. (Monad m) => m T.Text
            giveHelp = return textExplanation
        
        mainServer2 :: Server NewAPI
        mainServer2 = mainServer :<|> serveDirectoryFileServer "static"

        serveQueries1 :: Application
        serveQueries1 = serve (Proxy @MainAPI) mainServer

        runServerBasic1 :: Int -> IO ()
        runServerBasic1 pn = run pn serveQueries1

        runServer1 :: Int -> Int -> IO ()
        runServer1 pn timout = runSettings stg serveQueries1
          where stg = setTimeout timout $ setPort pn defaultSettings
        
        serveQueries2 :: Application
        serveQueries2 = serve (Proxy @NewAPI) mainServer2

        runServerBasic2 :: Int -> IO ()
        runServerBasic2 pn = run pn serveQueries2

        runServer2 :: Int -> Int -> IO ()
        runServer2 pn timout = runSettings stg serveQueries2
          where stg = setTimeout timout $ setPort pn defaultSettings

        serveQueries :: Application
        serveQueries
          | useRaw    = serveQueries2 
          | otherwise = serveQueries1

        runServer :: Int -> Int -> IO ()
        runServer pn timout
          | useRaw    = runServer2 pn timout
          | otherwise = runServer1 pn timout
        
        runServerBasic :: Int -> IO ()
        runServerBasic pn
          | useRaw    = runServerBasic2 pn
          | otherwise = runServerBasic1 pn

    |]
      
  return ()

  return mainDecs


textExplanation :: T.Text
textExplanation = T.unlines
  [ "Will have an actual explanation of how to use this"
  , "server later on. For now, try going to" 
  , "\"localhost:<portnumber>/info\"."
  ]


