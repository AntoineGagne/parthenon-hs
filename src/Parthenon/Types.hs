{-# LANGUAGE DeriveGeneric #-}

module Parthenon.Types
  ( Athena (..),
    Precision,
    Scale,
    Length,
  )
where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Int
import Data.Text (Text)
import GHC.Generics

type Precision = Int

type Scale = Int

type Length = Int

data Athena
  = AStruct [(Text, Athena)]
  | AArray [Athena]
  | ATinyInt Int8
  | ASmallInt Int16
  | AInt Int32
  | AString Text
  | ABigInt Int64
  | ADouble Double
  | AFloat Float
  | ABoolean Bool
  | AChar Length Text
  | ADecimal Precision Scale Double
  | ANull
  deriving (Eq, Generic, Show)

instance ToJSON Athena where
  toJSON (AStruct keyValues) = object [fromText key .= toJSON value | (key, value) <- keyValues]
  toJSON (AArray values) = toJSON $ map toJSON values
  toJSON (AInt value) = toJSON value
  toJSON (ATinyInt value) = toJSON value
  toJSON (ASmallInt value) = toJSON value
  toJSON (AString value) = toJSON value
  toJSON (ADouble value) = toJSON value
  toJSON (AFloat value) = toJSON value
  toJSON (ABigInt value) = toJSON value
  toJSON (ABoolean value) = toJSON value
  toJSON (ADecimal _ _ value) = toJSON value
  toJSON (AChar _ value) = toJSON value
  toJSON ANull = Null
