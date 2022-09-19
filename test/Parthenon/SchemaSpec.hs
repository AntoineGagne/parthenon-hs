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
    it "can decode an array of tiny integers" $
      parseMaybe "array<tinyint>" "[120, -120, null]"
        `shouldBe` Just (AArray [ATinyInt 120, ATinyInt (-120), ANull])
    it "can decode an array of small integers" $
      parseMaybe "array<smallint>" "[120, -120, null]"
        `shouldBe` Just (AArray [ASmallInt 120, ASmallInt (-120), ANull])
    it "can decode an array of big integers" $
      parseMaybe "array<bigint>" "[9999999999999999, -9999999999999999, null]"
        `shouldBe` Just (AArray [ABigInt 9999999999999999, ABigInt (-9999999999999999), ANull])
    it "can decode an array of strings" $
      parseMaybe "array<string>" "[foo, foo bar, null]"
        `shouldBe` Just (AArray [AString "foo", AString "foo bar", ANull])
    it "can decode an array of fixed strings" $
      parseMaybe "array<char(3)>" "[foo, bar, null]"
        `shouldBe` Just (AArray [AChar 3 "foo", AChar 3 "bar", ANull])
    it "can decode an array of doubles" $
      parseMaybe "array<double>" "[2.0, -2.0, null]"
        `shouldBe` Just (AArray [ADouble 2.0, ADouble (-2.0), ANull])
    it "can decode an array of floats" $
      parseMaybe "array<float>" "[2.0, -2.0, null]"
        `shouldBe` Just (AArray [AFloat 2.0, AFloat (-2.0), ANull])
    it "can decode an array of decimals" $
      parseMaybe "array<decimal(2, 5)>" "[2.0, -2.0, null]"
        `shouldBe` Just (AArray [ADecimal 2 5 2.0, ADecimal 2 5 (-2.0), ANull])
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
        "struct<a:int, b:string, c:bigint, d:boolean, e:double, f:array<int>, g:float, h:tinyint, i:smallint, j:char(2),k:decimal(10, 5)>"
        "{a=123, b=foo bar, c=123, d=true, e=-2.0, f=[123, null], g=1.1, h=18, i=24, j=ab, k=1.2}"
        `shouldBe` Just
          ( AStruct
              [ ("a", AInt 123),
                ("b", AString "foo bar"),
                ("c", ABigInt 123),
                ("d", ABoolean True),
                ("e", ADouble (-2.0)),
                ("f", AArray [AInt 123, ANull]),
                ("g", AFloat 1.1),
                ("h", ATinyInt 18),
                ("i", ASmallInt 24),
                ("j", AChar 2 "ab"),
                ("k", ADecimal 10 5 1.2)
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
    it "can decode a complex struct" $
      parseMaybe
        "struct<id:string,tmax:int,imp:array<struct<id:string,exp:int,video:struct<minduration:int,maxduration:int>,banner:struct<protocol:int,protocols:array<int>,w:int,h:int,ext:struct<id:string,ids:array<string>>>>>>"
        "{id=6a61e880-90a3-4421-80b0-150910cbfc74,tmax=80,imp=[{id=eea25994-cd64-4404-92ed-aa2ea0408316,exp=3600,banner={protocol=5,protocols=[1,2,3,4,5],w=5,h=10,ext={id=7e4b1fac-1d61-4933-8681-159188c56c63,ids=[61a51531-6e89-4ba9-bfea-7138ab6aec56, 5a4e23d2-0b41-4166-acd6-77202ce67c56]}}}]}"
        `shouldBe` Just
          ( AStruct
              [ ("id", AString "6a61e880-90a3-4421-80b0-150910cbfc74"),
                ("tmax", AInt 80),
                ( "imp",
                  AArray
                    [ AStruct
                        [ ("id", AString "eea25994-cd64-4404-92ed-aa2ea0408316"),
                          ("exp", AInt 3600),
                          ( "banner",
                            AStruct
                              [ ("protocol", AInt 5),
                                ("protocols", AArray [AInt 1, AInt 2, AInt 3, AInt 4, AInt 5]),
                                ("w", AInt 5),
                                ("h", AInt 10),
                                ( "ext",
                                  AStruct
                                    [ ("id", AString "7e4b1fac-1d61-4933-8681-159188c56c63"),
                                      ( "ids",
                                        AArray
                                          [ AString "61a51531-6e89-4ba9-bfea-7138ab6aec56",
                                            AString "5a4e23d2-0b41-4166-acd6-77202ce67c56"
                                          ]
                                      )
                                    ]
                                )
                              ]
                          )
                        ]
                    ]
                )
              ]
          )
    it "can decode a struct with string that contains commas" $
      parseMaybe
        "struct<b:string>"
        "{b=foo bar, jane smith}"
        `shouldBe` Just
          ( AStruct
              [ ("b", AString "foo bar, jane smith")
              ]
          )
    it "can decode a struct with string that contains commas and unknown keys" $
      parseMaybe
        "struct<b:string>"
        "{a=123, b=foo bar, jane smith, c=123, d=true, e=-2.0}"
        `shouldBe` Just
          ( AStruct
              [ ("a", AString "123"),
                ("b", AString "foo bar, jane smith"),
                ("c", AString "123"),
                ("d", AString "true"),
                ("e", AString "-2.0")
              ]
          )
    it "can decode a nested struct with string that contains commas and unknown keys" $
      parseMaybe
        "struct<a:struct<b:string>>"
        "{a={a=123, b=foo bar, jane smith, c=123, d=true, e=-2.0}}"
        `shouldBe` Just
          ( AStruct
              [ ( "a",
                  AStruct
                    [ ("a", AString "123"),
                      ("b", AString "foo bar, jane smith"),
                      ("c", AString "123"),
                      ("d", AString "true"),
                      ("e", AString "-2.0")
                    ]
                )
              ]
          )
    it "can decode a struct with string that contains equal symbols" $
      parseMaybe
        "struct<b:string>"
        "{b=foo=bar=}"
        `shouldBe` Just
          ( AStruct
              [ ("b", AString "foo=bar=")
              ]
          )
    it "can decode a struct with string that contains equal symbols and commas" $
      parseMaybe
        "struct<b:string>"
        "{b=foo=bar==,foo,bar}"
        `shouldBe` Just
          ( AStruct
              [ ("b", AString "foo=bar==,foo,bar")
              ]
          )

parseMaybe :: Text -> Text -> Maybe Athena
parseMaybe rawSchema input = do
  schema <- Megaparsec.parseMaybe Schema.schema rawSchema
  Megaparsec.parseMaybe schema input
