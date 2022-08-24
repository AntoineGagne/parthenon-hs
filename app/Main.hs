module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Options.Applicative (execParser)
import Parthenon (Options (..))
import qualified Parthenon
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>))
import Text.Megaparsec

main :: IO ()
main = do
  defaultDataDirectory <- getXdgDirectory XdgData ""
  invokedWith <- execParser (Parthenon.options defaultDataDirectory)
  let schemaDirectory = oSchemaDirectory invokedWith </> "parthenon"
      schema = schemaDirectory </> oSchema invokedWith
      input = oInput invokedWith
  createDirectoryIfMissing True schemaDirectory
  rawSchema <- TextIO.readFile schema
  rawAthena <- getAthena input
  case decode rawSchema rawAthena of
    Right parsed ->
      ByteString.putStr $ encode parsed
    Left error' ->
      putStr . errorBundlePretty $ error'
  where
    decode rawSchema rawAthena = do
      parser <- runParser Parthenon.schema "schema" rawSchema
      runParser parser "athena" rawAthena

    getAthena :: Text -> IO Text
    getAthena raw = case raw of
      "-" ->
        TextIO.getContents
      other ->
        pure other
