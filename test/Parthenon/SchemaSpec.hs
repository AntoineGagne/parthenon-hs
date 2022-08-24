module Parthenon.SchemaSpec where

import Data.Text (Text)
import qualified Parthenon.Schema as Schema
import Parthenon.Types (Athena (..))
import Test.Hspec
import qualified Text.Megaparsec as Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "array" $
    it "can decode an empty array" $
      parseMaybe "array<int>" "[]" `shouldBe` Just (AArray [])
  describe "struct" $
    it "can decode an empty struct" $
      parseMaybe "struct<a:int>" "{}" `shouldBe` Just (AStruct [])

parseMaybe :: Text -> Text -> Maybe Athena
parseMaybe rawSchema input = do
  schema <- Megaparsec.parseMaybe Schema.schema rawSchema
  Megaparsec.parseMaybe schema input
