{-# LANGUAGE TemplateHaskell #-}

module Metamorth.Server.CLI
  ( makeCLI ) where

import Language.Haskell.TH

import Metamorth.Server.ForOutput.CLI
import Metamorth.Server.Helpers

import Options.Applicative

makeCLI :: String -> String -> Q [Dec]
makeCLI progName langName = do
  
  runName <- maybeLookupValue "runServer" "Couldn't find the \"runServer\" function.\nMake sure it's exported."

  let progNameQ = pure $ LitE $ StringL progName
      langNameQ = pure $ LitE $ StringL langName

  [d|
    main :: IO ()
    main = runServer' =<< execParser opts
      where 
        opts = info (progOptParser <**> helper)
               (  fullDesc
               <> progDesc ("A server-based orthography converter for " <> $langNameQ)
               <> header   ($progNameQ <> " - A server for converting between orthographies of " <> $langNameQ)
               )
        runServer' :: ProgramOptions -> IO ()
        runServer' props = $(pure $ VarE runName) (progPort props) (progTimeout props)
   |]


