{-# LANGUAGE DeriveGeneric #-}

module Parthenon.Types
  ( Athena (..),
  )
where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Text (Text)
import GHC.Generics

data Athena
  = AStruct [(Text, Athena)]
  | AArray [Athena]
  | AInt Int
  | AString Text
  | ABigInt Integer
  | ADouble Double
  | ABoolean Bool
  | ANull
  deriving (Eq, Generic, Show)

instance ToJSON Athena where
  toJSON (AStruct keyValues) = object [fromText key .= toJSON value | (key, value) <- keyValues]
  toJSON (AArray values) = toJSON $ map toJSON values
  toJSON (AInt value) = toJSON value
  toJSON (AString value) = toJSON value
  toJSON (ADouble value) = toJSON value
  toJSON (ABigInt value) = toJSON value
  toJSON (ABoolean value) = toJSON value
  toJSON ANull = Null
