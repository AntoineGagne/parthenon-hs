module Parthenon.Config.Cli
  ( options,
    Options (..),
  )
where

import Data.Text (Text)
import Options.Applicative

data Options = Options
  { oSchemaDirectory :: FilePath,
    oPrettyPrint :: Bool,
    oSchema :: FilePath,
    oInput :: Text
  }
  deriving (Show, Eq)

options :: FilePath -> ParserInfo Options
options directory =
  info
    (options' <**> helper)
    ( fullDesc
        <> progDesc "Convert Athena structures into JSON"
        <> header "parthenon - An Athena structure converter"
    )
  where
    options' :: Parser Options
    options' =
      Options
        <$> strOption
          ( long "schema-directory"
              <> short 'd'
              <> value directory
              <> metavar "DIRECTORY"
              <> help "Specify the directory where the schema is located"
          )
        <*> switch
          ( long "pretty-print"
              <> short 'p'
              <> help "Pretty-print the JSON"
          )
        <*> strArgument (metavar "SCHEMA")
        <*> strArgument
          ( metavar "INPUT"
              <> value "-"
          )
