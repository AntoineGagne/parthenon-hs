module Parthenon.DecoderSpec where

import Data.Maybe
  ( fromJust,
    isJust,
  )
import Data.Text (Text)
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
      parseMaybe Decoder.double "2.0" `shouldBe` Just (ADouble (Just 2.0))
    it "can decode negative double" $
      parseMaybe Decoder.double "-2.0" `shouldBe` Just (ADouble (Just (-2.0)))
    it "can decode null double" $
      parseMaybe Decoder.double "null" `shouldBe` Just (ADouble Nothing)
  describe "boolean" $ do
    it "can decode \"true\"" $
      parseMaybe Decoder.boolean "true" `shouldBe` Just (ABoolean (Just True))
    it "can decode \"false\"" $
      parseMaybe Decoder.boolean "false" `shouldBe` Just (ABoolean (Just False))
    it "can decode null boolean" $
      parseMaybe Decoder.boolean "null" `shouldBe` Just (ABoolean Nothing)
  describe "bigInt" $ do
    it "can decode a big integer" $
      parseMaybe Decoder.bigInt "2000000" `shouldBe` Just (ABigInt (Just 2000000))
    it "can decode a negative big integer" $
      parseMaybe Decoder.bigInt "-2000000" `shouldBe` Just (ABigInt (Just (-2000000)))
    it "can decode a null big integer" $
      parseMaybe Decoder.bigInt "null" `shouldBe` Just (ABigInt Nothing)
  describe "integer" $ do
    it "can decode an integer" $
      parseMaybe Decoder.integer "42" `shouldBe` Just (AInt (Just 42))
    it "can decode negative integer" $
      parseMaybe Decoder.integer "-42" `shouldBe` Just (AInt (Just (-42)))
    it "can decode null integer" $
      parseMaybe Decoder.integer "null" `shouldBe` Just (AInt Nothing)
  describe "array" $ do
    it "can decode an array of integer" $
      parseMaybe (Decoder.array Decoder.integer) "[42]"
        `shouldBe` Just (AArray (Just [Decoder.AInt (Just 42)]))
    it "can decode an array of big integer" $
      parseMaybe (Decoder.array Decoder.bigInt) "[42, 7]"
        `shouldBe` Just
          ( AArray
              (Just [ABigInt (Just 42), Decoder.ABigInt (Just 7)])
          )
    it "can decode an array of simple strings" $
      parseMaybe (Decoder.array Decoder.string) "[foo, bar]"
        `shouldBe` Just
          ( AArray
              (Just [AString (Just "foo"), Decoder.AString (Just "bar")])
          )
    it "can decode an array of strings with spaces" $
      parseMaybe (Decoder.array Decoder.string) "[foo hello, bar world]"
        `shouldBe` Just
          ( AArray
              ( Just
                  ( [ AString (Just "foo hello"),
                      AString (Just "bar world")
                    ]
                  )
              )
          )
    it "can decode an array of strings with special characters" $
      parseMaybe (Decoder.array Decoder.string) "[application/json, application/text]"
        `shouldBe` Just
          ( AArray
              ( Just
                  ( [ AString (Just "application/json"),
                      AString (Just "application/text")
                    ]
                  )
              )
          )
    it "can decode an array of boolean" $
      parseMaybe (Decoder.array Decoder.boolean) "[true, false]"
        `shouldBe` Just
          ( AArray
              ( Just ([ABoolean (Just True), Decoder.ABoolean (Just False)])
              )
          )
    it "can decode an array with null values" $
      parseMaybe (Decoder.array Decoder.boolean) "[null, null, true]"
        `shouldBe` Just
          ( AArray
              ( Just
                  ( [ ABoolean Nothing,
                      ABoolean Nothing,
                      ABoolean (Just True)
                    ]
                  )
              )
          )
    it "can decode a null array" $
      parseMaybe (Decoder.array Decoder.boolean) "null"
        `shouldBe` Just (AArray Nothing)
  describe "struct" $ do
    it "can decode an empty struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "{}"
        `shouldBe` Just (AStruct (Just []))
    it "can decode a null struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "null"
        `shouldBe` Just (AStruct Nothing)
    it "can decode a flat struct" $
      parseMaybe (Decoder.struct [("a", Decoder.integer)]) "{a=1}"
        `shouldBe` Just (AStruct (Just [("a", Decoder.AInt (Just 1))]))
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
              ( Just
                  ( [ ("a", AInt (Just 1)),
                      ("b", AString (Just "foo")),
                      ("c", ABoolean (Just True)),
                      ("d", ADouble (Just 2.0)),
                      ("e", ABigInt (Just 2000))
                    ]
                  )
              )
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
              ( Just
                  ( [ ("a", AInt (Just 1)),
                      ("b", AInt (Just 5))
                    ]
                  )
              )
          )
    it "can decode a struct with array" $
      parseMaybe (Decoder.struct [("a", Decoder.array Decoder.integer)]) "{a=[1, 2, 3, 4]}"
        `shouldBe` Just
          ( AStruct
              ( Just
                  ( [ ( "a",
                        AArray
                          ( Just
                              [ AInt (Just 1),
                                AInt (Just 2),
                                AInt (Just 3),
                                AInt (Just 4)
                              ]
                          )
                      )
                    ]
                  )
              )
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
              ( Just
                  ( [ ( "a",
                        AArray
                          ( Just
                              [ AStruct
                                  ( Just
                                      [ ( "b",
                                          AArray
                                            ( Just
                                                [ AInt (Just 1),
                                                  AInt (Just 2),
                                                  AInt (Just 3),
                                                  AInt (Just 4)
                                                ]
                                            )
                                        )
                                      ]
                                  )
                              ]
                          )
                      )
                    ]
                  )
              )
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
              ( Just
                  ( [ ( "a",
                        AString (Just "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36")
                      )
                    ]
                  )
              )
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
              ( Just
                  ( [ ( "a",
                        AString (Just "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36")
                      ),
                      ( "b",
                        AInt (Just 3)
                      )
                    ]
                  )
              )
          )
