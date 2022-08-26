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
  describe "array" $ do
    it "can decode an empty array" $
      parseMaybe "array<int>" "[]" `shouldBe` Just (AArray [])
    it "can decode a null array" $
      parseMaybe "array<int>" "null" `shouldBe` Just ANull
    it "can decode an array of integers" $
      parseMaybe "array<int>" "[123, -456, null]"
        `shouldBe` Just (AArray [AInt 123, AInt (-456), ANull])
    it "can decode an array of big integers" $
      parseMaybe "array<bigint>" "[9999999999999999, -9999999999999999, null]"
        `shouldBe` Just (AArray [ABigInt 9999999999999999, ABigInt (-9999999999999999), ANull])
    it "can decode an array of strings" $
      parseMaybe "array<string>" "[foo, foo bar, null]"
        `shouldBe` Just (AArray [AString "foo", AString "foo bar", ANull])
    it "can decode an array of doubles" $
      parseMaybe "array<double>" "[2.0, -2.0, null]"
        `shouldBe` Just (AArray [ADouble 2.0, ADouble (-2.0), ANull])
    it "can decode an array of booleans" $
      parseMaybe "array<boolean>" "[true, false, null]"
        `shouldBe` Just (AArray [ABoolean True, ABoolean False, ANull])
    it "can decode array of structs" $
      parseMaybe "array<struct<a:int, b:string>>" "[null, {a=123, b=foo/bar}]"
        `shouldBe` Just (AArray [ANull, AStruct [("a", AInt 123), ("b", AString "foo/bar")]])
    it "can decode nested arrays" $
      parseMaybe "array<array<boolean>>" "[[true, false, null]]"
        `shouldBe` Just (AArray [AArray [ABoolean True, ABoolean False, ANull]])
  describe "struct" $ do
    it "can decode an empty struct" $
      parseMaybe "struct<a:int>" "{}" `shouldBe` Just (AStruct [])
    it "can decode a null struct" $
      parseMaybe "struct<a:int>" "null" `shouldBe` Just ANull
    it "can decode a struct with every types" $
      parseMaybe
        "struct<a:int, b:string, c:bigint, d:boolean, e:double, f:array<int>>"
        "{a=123, b=foo bar, c=123, d=true, e=-2.0, f=[123, null]}"
        `shouldBe` Just
          ( AStruct
              [ ("a", AInt 123),
                ("b", AString "foo bar"),
                ("c", ABigInt 123),
                ("d", ABoolean True),
                ("e", ADouble (-2.0)),
                ("f", AArray [AInt 123, ANull])
              ]
          )
    it "can decode a nested struct" $
      parseMaybe
        "struct<a:struct<a:int, b:string, c:bigint, d:boolean, e:double>>"
        "{a={a=123, b=foo bar, c=123, d=true, e=-2.0}}"
        `shouldBe` Just
          ( AStruct
              [ ( "a",
                  AStruct
                    [ ("a", AInt 123),
                      ("b", AString "foo bar"),
                      ("c", ABigInt 123),
                      ("d", ABoolean True),
                      ("e", ADouble (-2.0))
                    ]
                )
              ]
          )

parseMaybe :: Text -> Text -> Maybe Athena
parseMaybe rawSchema input = do
  schema <- Megaparsec.parseMaybe Schema.schema rawSchema
  Megaparsec.parseMaybe schema input
