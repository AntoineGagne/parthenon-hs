module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Options.Applicative (customExecParser)
import Parthenon
  ( FileArgument (..),
    InputArgument (..),
    Options (..),
  )
import qualified Parthenon
import Text.Megaparsec

main :: IO ()
main = do
  invokedWith <- customExecParser Parthenon.preferences Parthenon.options
  let schema = oSchema invokedWith
      input = oInput invokedWith
  rawSchema <- fromFileArgument schema
  rawAthena <- fromInputArgument input
  case decode rawSchema rawAthena of
    Right parsed ->
      ByteString.putStr $ encode parsed
    Left error' ->
      putStr . errorBundlePretty $ error'
  where
    decode rawSchema rawAthena = do
      parser <- runParser Parthenon.schema "schema" rawSchema
      runParser parser "athena" rawAthena

    fromFileArgument :: FileArgument -> IO Text
    fromFileArgument raw = case raw of
      FFromFile filepath ->
        TextIO.readFile filepath
      FFromArgument other ->
        pure other

    fromInputArgument :: InputArgument -> IO Text
    fromInputArgument raw = case raw of
      IFromStdin ->
        TextIO.getContents
      IFromFile filepath ->
        TextIO.readFile filepath
      IFromArgument other ->
        pure other
