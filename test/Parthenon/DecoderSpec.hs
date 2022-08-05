module Parthenon.DecoderSpec where

import Data.Maybe
  ( fromJust,
    isJust,
  )
import Data.Text (Text)
import qualified Parthenon.Decoder as Decoder
import Test.Hspec
import Text.Megaparsec (parseMaybe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "double" $ do
    it "can decode a double" $
      parseMaybe Decoder.double "2.0" `shouldBe` Just (Decoder.ADouble 2.0)
    it "can decode negative double" $
      parseMaybe Decoder.double "-2.0" `shouldBe` Just (Decoder.ADouble (-2.0))
  describe "boolean" $ do
    it "can parse \"true\"" $
      parseMaybe Decoder.boolean "true" `shouldBe` Just (Decoder.ABoolean True)
    it "can parse \"false\"" $
      parseMaybe Decoder.boolean "false" `shouldBe` Just (Decoder.ABoolean False)
  describe "bigInt" $ do
    it "can decode a big integer" $
      parseMaybe Decoder.bigInt "2000000" `shouldBe` Just (Decoder.ABigInt 2000000)
    it "can decode a negative big integer" $
      parseMaybe Decoder.bigInt "-2000000" `shouldBe` Just (Decoder.ABigInt (-2000000))
  describe "integer" $ do
    it "can decode an integer" $
      parseMaybe Decoder.integer "42" `shouldBe` Just (Decoder.AInt 42)
    it "can decode negative integer" $
      parseMaybe Decoder.integer "-42" `shouldBe` Just (Decoder.AInt (-42))
  describe "array" $ do
    it "can decode an array of integer" $
      parseMaybe (Decoder.array Decoder.integer) "[42]" `shouldBe` Just (Decoder.AArray [Decoder.AInt 42])
    it "can decode an array of big integer" $
      parseMaybe (Decoder.array Decoder.bigInt) "[42, 7]" `shouldBe` Just (Decoder.AArray [Decoder.ABigInt 42, Decoder.ABigInt 7])
    it "can decode an array of simple strings" $
      parseMaybe (Decoder.array Decoder.string) "[foo, bar]" `shouldBe` Just (Decoder.AArray [Decoder.AString "foo", Decoder.AString "bar"])
    it "can decode an array of strings with spaces" $
      parseMaybe (Decoder.array Decoder.string) "[foo hello, bar world]"
        `shouldBe` Just
          ( Decoder.AArray
              [ Decoder.AString "foo hello",
                Decoder.AString "bar world"
              ]
          )
    it "can decode an array of strings with special characters" $
      parseMaybe (Decoder.array Decoder.string) "[application/json, application/text]"
        `shouldBe` Just
          ( Decoder.AArray
              [ Decoder.AString "application/json",
                Decoder.AString "application/text"
              ]
          )
    it "can decode an array of boolean" $
      parseMaybe (Decoder.array Decoder.boolean) "[true, false]" `shouldBe` Just (Decoder.AArray [Decoder.ABoolean True, Decoder.ABoolean False])
  describe "struct" $ do
    it "can decode an empty struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "{}" `shouldBe` Just (Decoder.AStruct [])
    it "can decode a flat struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "{a=1}" `shouldBe` Just (Decoder.AStruct [("a", Decoder.AInt 1)])
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
          ( Decoder.AStruct
              [ ("a", Decoder.AInt 1),
                ("b", Decoder.AString "foo"),
                ("c", Decoder.ABoolean True),
                ("d", Decoder.ADouble 2.0),
                ("e", Decoder.ABigInt 2000)
              ]
          )
    it "can decode a flat struct with spaces" $
      parseMaybe
        ( Decoder.struct
            [ ("a", Decoder.integer),
              ("b", Decoder.integer)
            ]
        )
        "{ a = 1, b = 5 }"
        `shouldBe` Just
          ( Decoder.AStruct
              [ ("a", Decoder.AInt 1),
                ("b", Decoder.AInt 5)
              ]
          )
    it "can decode a struct with array" $
      parseMaybe (Decoder.struct [("a", Decoder.array Decoder.integer)]) "{a=[1, 2, 3, 4]}"
        `shouldBe` Just
          ( Decoder.AStruct
              [ ( "a",
                  Decoder.AArray
                    [ Decoder.AInt 1,
                      Decoder.AInt 2,
                      Decoder.AInt 3,
                      Decoder.AInt 4
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
          ( Decoder.AStruct
              [ ( "a",
                  Decoder.AArray
                    [ Decoder.AStruct
                        [ ( "b",
                            Decoder.AArray
                              [ Decoder.AInt 1,
                                Decoder.AInt 2,
                                Decoder.AInt 3,
                                Decoder.AInt 4
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
          ( Decoder.AStruct
              [ ( "a",
                  Decoder.AString "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"
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
          ( Decoder.AStruct
              [ ( "a",
                  Decoder.AString "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"
                ),
                ( "b",
                  Decoder.AInt 3
                )
              ]
          )
