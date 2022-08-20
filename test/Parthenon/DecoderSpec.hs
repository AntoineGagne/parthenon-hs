module Parthenon.DecoderSpec where

import qualified Parthenon.Decoder as Decoder
import Parthenon.Types (Athena (..))
import Test.Hspec
import Text.Megaparsec (parseMaybe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "double" $ do
    it "can decode a double" $
      parseMaybe Decoder.double "2.0" `shouldBe` Just (ADouble 2.0)
    it "can decode negative double" $
      parseMaybe Decoder.double "-2.0" `shouldBe` Just (ADouble (-2.0))
    it "can decode null double" $
      parseMaybe Decoder.double "null" `shouldBe` Just ANull
  describe "boolean" $ do
    it "can decode \"true\"" $
      parseMaybe Decoder.boolean "true" `shouldBe` Just (ABoolean True)
    it "can decode \"false\"" $
      parseMaybe Decoder.boolean "false" `shouldBe` Just (ABoolean False)
    it "can decode null boolean" $
      parseMaybe Decoder.boolean "null" `shouldBe` Just ANull
  describe "bigInt" $ do
    it "can decode a big integer" $
      parseMaybe Decoder.bigInt "2000000" `shouldBe` Just (ABigInt 2000000)
    it "can decode a negative big integer" $
      parseMaybe Decoder.bigInt "-2000000" `shouldBe` Just (ABigInt (-2000000))
    it "can decode a null big integer" $
      parseMaybe Decoder.bigInt "null" `shouldBe` Just ANull
  describe "integer" $ do
    it "can decode an integer" $
      parseMaybe Decoder.integer "42" `shouldBe` Just (AInt 42)
    it "can decode negative integer" $
      parseMaybe Decoder.integer "-42" `shouldBe` Just (AInt (-42))
    it "can decode null integer" $
      parseMaybe Decoder.integer "null" `shouldBe` Just ANull
  describe "string" $ do
    it "can decode string" $
      parseMaybe Decoder.string "foo" `shouldBe` Just (AString "foo")
    it "can decode null string" $
      parseMaybe Decoder.string "null" `shouldBe` Just ANull
    it "can decode a string with spaces" $
      parseMaybe Decoder.string "some spaces in between"
        `shouldBe` Just (AString "some spaces in between")
    it "can decode a string with special characters" $
      parseMaybe Decoder.string "(some/spaces\\in|between)"
        `shouldBe` Just (AString "(some/spaces\\in|between)")
  describe "array" $ do
    it "can decode an array of integer" $
      parseMaybe (Decoder.array Decoder.integer) "[42]"
        `shouldBe` Just (AArray [Decoder.AInt 42])
    it "can decode an array of big integer" $
      parseMaybe (Decoder.array Decoder.bigInt) "[42, 7]"
        `shouldBe` Just
          ( AArray
              [ABigInt 42, ABigInt 7]
          )
    it "can decode an array of simple strings" $
      parseMaybe (Decoder.array Decoder.string) "[foo, bar]"
        `shouldBe` Just
          ( AArray
              [AString "foo", AString "bar"]
          )
    it "can decode an array of strings with spaces" $
      parseMaybe (Decoder.array Decoder.string) "[foo hello, bar world]"
        `shouldBe` Just
          ( AArray
              [ AString "foo hello",
                AString "bar world"
              ]
          )
    it "can decode an array of strings with special characters" $
      parseMaybe (Decoder.array Decoder.string) "[application/json, application/text]"
        `shouldBe` Just
          ( AArray
              [ AString "application/json",
                AString "application/text"
              ]
          )
    it "can decode an array of boolean" $
      parseMaybe (Decoder.array Decoder.boolean) "[true, false]"
        `shouldBe` Just
          ( AArray
              [ABoolean True, ABoolean False]
          )
    it "can decode an array with null values" $
      parseMaybe (Decoder.array Decoder.boolean) "[null, null, true]"
        `shouldBe` Just
          ( AArray
              [ ANull,
                ANull,
                ABoolean True
              ]
          )
    it "can decode a null array" $
      parseMaybe (Decoder.array Decoder.boolean) "null"
        `shouldBe` Just ANull
  describe "struct" $ do
    it "can decode an empty struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "{}"
        `shouldBe` Just (AStruct [])
    it "can decode a null struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "null"
        `shouldBe` Just ANull
    it "can decode a flat struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "{a=1}"
        `shouldBe` Just (AStruct [("a", AInt 1)])
    it "can decode a flat struct with multiple fields" $
      parseMaybe
        ( Decoder.struct
            [ ("a", Decoder.integer),
              ("b", Decoder.string),
              ("c", Decoder.boolean),
              ("d", Decoder.double),
              ("e", Decoder.bigInt)
            ]
        )
        "{a=1,b=foo,c=true,d=2.0,e=2000}"
        `shouldBe` Just
          ( AStruct
              [ ("a", AInt 1),
                ("b", AString "foo"),
                ("c", ABoolean True),
                ("d", ADouble 2.0),
                ("e", ABigInt 2000)
              ]
          )
    it "can decode a flat struct with spaces" $
      parseMaybe
        ( Decoder.struct
            [ ("a", Decoder.integer),
              ("b", Decoder.integer)
            ]
        )
        "{a = 1, b = 5}"
        `shouldBe` Just
          ( AStruct
              [ ("a", AInt 1),
                ("b", AInt 5)
              ]
          )
    it "can decode a struct with array" $
      parseMaybe (Decoder.struct [("a", Decoder.array Decoder.integer)]) "{a=[1, 2, 3, 4]}"
        `shouldBe` Just
          ( AStruct
              [ ( "a",
                  AArray
                    [ AInt 1,
                      AInt 2,
                      AInt 3,
                      AInt 4
                    ]
                )
              ]
          )
    it "can decode a struct with array of structs" $
      parseMaybe
        ( Decoder.struct
            [ ( "a",
                Decoder.array (Decoder.struct [("b", Decoder.array Decoder.integer)])
              )
            ]
        )
        "{a = [{b = [1, 2, 3, 4]}]}"
        `shouldBe` Just
          ( AStruct
              [ ( "a",
                  AArray
                    [ AStruct
                        [ ( "b",
                            AArray
                              [ AInt 1,
                                AInt 2,
                                AInt 3,
                                AInt 4
                              ]
                          )
                        ]
                    ]
                )
              ]
          )
    it "can decode a struct with string that contain commas, spaces and special characters" $
      parseMaybe
        ( Decoder.struct
            [ ("a", Decoder.string)
            ]
        )
        "{a = Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36}"
        `shouldBe` Just
          ( AStruct
              [ ( "a",
                  AString "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"
                )
              ]
          )
    it "can decode a struct with multiple fields with string that contain commas, spaces and special characters" $
      parseMaybe
        ( Decoder.struct
            [ ("a", Decoder.string),
              ("b", Decoder.integer)
            ]
        )
        "{a = Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36, b=3}"
        `shouldBe` Just
          ( AStruct
              [ ( "a",
                  AString "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"
                ),
                ( "b",
                  AInt 3
                )
              ]
          )
