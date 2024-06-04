module Metamorth.Server.ForOutput.CLI
  ( ProgramOptions(..)
  , defOptions
  , progOptParser
  ) where

import Options.Applicative

data ProgramOptions = ProgramOptions
  { progPort    :: Int
  , progTimeout :: Int
  } deriving (Show, Eq)

defOptions :: ProgramOptions
defOptions = ProgramOptions 8081 30

progOptParser :: Parser ProgramOptions
progOptParser = ProgramOptions
  <$> option auto
      ( long "port"
      <> short 'p'
      <> help "The port number the server will use"
      <> showDefault
      <> value 8081
      <> metavar "PORT"
      )
  <*> option auto
      ( long "timeout"
      <> long "timeout"
      <> short 't'
      <> help "Timeout (in seconds) for the server"
      <> showDefault
      <> value 30
      <> metavar "TIME"
      )

