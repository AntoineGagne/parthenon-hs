module Parthenon.Config.Cli
  ( options,
    preferences,
    FileArgument (..),
    InputArgument (..),
    Options (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as Pretty
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char

data Options = Options
  { oSchema :: FileArgument,
    oInput :: InputArgument
  }
  deriving (Show, Eq)

type OptionParser = Parsec Void String

data FileArgument
  = FFromFile FilePath
  | FFromArgument Text
  deriving (Eq, Show)

data InputArgument
  = IFromFile FilePath
  | IFromArgument Text
  | IFromStdin
  deriving (Eq, Show)

preferences :: ParserPrefs
preferences = prefs $ showHelpOnEmpty <> showHelpOnError <> helpLongEquals

options :: ParserInfo Options
options =
  info
    (options' <**> helper)
    ( fullDesc
        <> progDesc "Convert Athena structures into JSON"
        <> header "parthenon - An Athena structure converter"
        <> footerDoc (Just examples)
    )
  where
    examples =
      Pretty.hang
        2
        ( Pretty.vcat
            [ "Examples:",
              "$ echo '{a=1234, b=foo bar}' | parthenon @file-containing-schema -",
              "$ echo '{a=1234, b=foo bar}' | parthenon 'struct<a:int, b:string>' -",
              "$ parthenon 'struct<a:int, b:string>' '{a=1234, b=foo bar}'",
              "$ parthenon 'struct<a:int, b:string>' @file-containing-athena-term"
            ]
        )

    options' :: Parser Options
    options' =
      Options
        <$> Options.Applicative.argument fileArgument (metavar "SCHEMA")
        <*> Options.Applicative.argument
          inputArgument
          ( metavar "INPUT"
              <> value IFromStdin
          )

    fileArgument :: ReadM FileArgument
    fileArgument =
      maybeReader (parseMaybe fileArgument')
      where
        fileArgument' = try (fromFile <|> fromArgument)

        fromFile :: OptionParser FileArgument
        fromFile = do
          _ <- Char.char '@'
          FFromFile <$> takeRest

        fromArgument :: OptionParser FileArgument
        fromArgument = FFromArgument . Text.pack <$> takeRest

    inputArgument :: ReadM InputArgument
    inputArgument = maybeReader (parseMaybe fileArgument')
      where
        fileArgument' = try (fromFile <|> fromStdin <|> fromArgument)

        fromStdin :: OptionParser InputArgument
        fromStdin = do
          Char.char '-'
          eof
          pure IFromStdin

        fromFile :: OptionParser InputArgument
        fromFile = do
          _ <- Char.char '@'
          IFromFile <$> takeRest

        fromArgument :: OptionParser InputArgument
        fromArgument = IFromArgument . Text.pack <$> takeRest
